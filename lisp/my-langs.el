;;; my-langs.el --- Language configurations -*- lexical-binding: t; -*-

;;; Eglot (built-in)

(use-package eglot
  :ensure nil
  :bind (:map eglot-mode-map
	      ("M-RET" . eglot-code-actions)
	      ("C-=" . #'eglot-momentary-inlay-hints))
  :hook
  (eglot-managed-mode . (lambda ()
                          (eglot-inlay-hints-mode -1)
                          (eglot-semantic-tokens-mode -1)))
  (c-ts-mode . my/maybe-eglot)
  (c++-ts-mode . my/maybe-eglot)
  (python-ts-mode . my/maybe-eglot)
  (zig-ts-mode . my/maybe-eglot)
  (bazel-mode . my/maybe-eglot)
  :config
  (add-to-list 'eglot-server-programs '(python-base-mode . ("ruff" "server")))
  (add-to-list 'eglot-server-programs
               `(zig-ts-mode . ,(lambda (&optional _interactive _project)
                                  (let* ((proj (project-current))
                                         (root (and proj (project-root proj)))
                                         (local (and root (expand-file-name "tools/zls.sh" root))))
                                    (unless (and local (file-executable-p local))
                                      (error "zls.sh not found at %s" local))
                                    (list local)))))
  (add-to-list 'eglot-server-programs '(bazel-mode . ("bazel-lsp")))
  (add-to-list 'eglot-server-programs '(v-mode . ("vls"))))

;;; Python

(use-package apheleia
  :config
  (apheleia-global-mode 1)
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(use-package flymake-ruff
  :hook (python-mode . flymake-ruff-load))

;;; Zig

(use-package zig-ts-mode
  :vc (:url "https://codeberg.org/meow_king/zig-ts-mode")
  :mode ("\\.zig\\'" . zig-ts-mode))

;;; Bazel

(use-package bazel
  :bind (:map bazel-mode-map
	      ("C-c C-c" . my-project-compile-project)
	      ("C-c C-v" . my-project-run-project))
  :mode ("\\.bazel\\'" . bazel-mode))

;;; V

(use-package v-mode
  :vc (:url "https://github.com/elogir/v-mode")
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . v-mode))

;;; Markdown

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

;;; Cuda

(use-package cuda-mode)

;;; Compilation

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(provide 'my-langs)
;;; my-langs.el ends here
