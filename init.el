(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'doom-text)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (require 'elpaca-use-package)
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;; Keep emacs Custom-settings in separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; custom functions
(defun open-emacs-config ()
  "Open the Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun my-projectile-run-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-run-command (projectile-compilation-dir)))
             prompt)))
    (projectile-run-project prompt)))

(defun my-projectile-compile-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-compilation-command (projectile-compilation-dir)))
             prompt)))
    (projectile-compile-project prompt)))

(defun my-setup-prog-keybindings ()
  "Set up keybindings for programming modes."
  (general-define-key
   :keymaps 'local
   "C-c C-c" 'my-projectile-compile-project
   "C-c C-v" 'my-projectile-run-project))

(add-hook 'prog-mode-hook #'my-setup-prog-keybindings)

;; global modes
(which-key-mode t)
(electric-pair-mode t)
(winner-mode t)
(delete-selection-mode t)
(global-auto-revert-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key [remap list-buffers] 'ibuffer)
(setq eshell-banner-message "")
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; general config
(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys)
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   "C-M-i" nil
   "M-<tab>" nil)
  (general-def
    [remap move-beginning-of-line] 'doom/backward-to-bol-or-indent
    [remap move-end-of-line] 'doom/forward-to-last-non-comment-or-eol
    "M-<tab>" nil
    "C-M-i" nil)
  (general-create-definer cc-def ; Comp prefix
    :prefix "C-c")
  (cc-def
    "<left>" 'winner-undo
    "<right>" 'winner-redo
    "o t" 'eshell)
  (general-create-definer cx-def ; Main prefix
    :prefix "C-x")
  (cx-def "K" 'kill-current-buffer)
  (general-create-definer ch-def ; Help prefix
    :prefix "C-h")
  (ch-def "d c" 'open-emacs-config)
  (general-define-key :keymaps 'prog-mode
		      "C-c C-c" 'my-projectile-compile-project
		      "C-c C-v" 'my-projectile-run-project))

;; packages

(use-package transient)

(use-package magit
  :after transient
  :general
  (cx "g" 'magit-status))

(use-package crux
  :general
  (general-def
    "M-o" 'crux-smart-open-line-above
    "C-o" 'crux-smart-open-line))

(use-package pulsar
  :config (pulsar-global-mode t))

(use-package ace-window
  :general
  (cx-def "o" 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ctrlf
  :config
  (ctrlf-mode t))

(use-package flycheck
  :custom (global-flycheck-mode t))

(use-package emojify
  :custom (global-emojify-mode t))

(use-package nerd-icons)

(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
				       if_statement with_statement while_statement)))
  (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)))
  :hook ((python-ts-mode yaml-mode) . indent-bars-mode))

(use-package apheleia
  :config (apheleia-global-mode t))

;; (use-package fancy-compilation
;;   :commands (fancy-compilation-mode))

;; (with-eval-after-load 'compile
;;   (fancy-compilation-mode))

(use-package eat
  :ensure (:type git :host codeberg :repo "akib/emacs-eat")
  :hook (eshell-load . eat-eshell-mode))

(use-package popper
  :general
  (general-def
    "C-`" 'popper-toggle
    "C-<tab>" 'popper-cycle)
  :init
  (setq popper-reference-buffers
        '("\\*eshell\\*"))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Welcome home")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(use-package quickrun
  :general
  (cc-def "r" 'quickrun))

(use-package goto-line-preview
  :general (general-def [remap goto-line] 'goto-line-preview))

(use-package yasnippet
  :general
  (cc-def "SPC" 'yas-expand)
  :config
  (yas-global-mode t)
  (general-define-key :keymaps 'yas-minor-mode-map "<tab>" nil))

(use-package yasnippet-capf
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package corfu
  :config
  (global-corfu-mode t)
  :general
  (general-def
    "M-`" 'completion-at-point))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package rg)

(use-package projectile
  :general
  (general-def
    "C-c p" 'projectile-command-map))

(use-package persp-mode) ; not enabled yet

(use-package good-scroll
  :config
  (good-scroll-mode 1))

;; Zig packages

(use-package zig-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/zig-ts-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode)))

(provide 'init)

;;; init.el ends here
