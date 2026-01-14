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

(defvar popper--saved-sizes (make-hash-table :test 'equal))

(defun popper--save-size (buffer)
  (when-let ((window (get-buffer-window buffer)))
    (puthash (buffer-name buffer) (window-height window) popper--saved-sizes)))

(defun popper--restore-size (buffer)
  (when-let* ((height (gethash (buffer-name buffer) popper--saved-sizes))
              (window (get-buffer-window buffer)))
    (window-resize window (- height (window-height window)))))

(defun popper--close-advice (orig-fun)
  (when popper-open-popup-alist
    (popper--save-size (cdar popper-open-popup-alist)))
  (funcall orig-fun))

(defun popper--open-hook ()
  (popper--restore-size (current-buffer)))


(defun open-emacs-config ()
  "Open the Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun my-new-eshell ()
  "Open a new eshell buffer with a unique name."
  (interactive)
  (let ((eshell-buffer-name (generate-new-buffer-name "*eshell*")))
    (eshell)))

(defun my-project-run-project (&optional prompt)
  "Run a command in the project root directory.
With prefix argument PROMPT, always prompt for the command.
Runs in comint-mode and makes the buffer writable."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (default-directory root)
         ;; If PROMPT is non-nil or no cached command, prompt.
         (compilation-read-command
          (or prompt
              (not (and (boundp 'my-project-run-command-cache)
                        (gethash root my-project-run-command-cache)))))
         ;; Allow input to the running process in comint buffer.
         (compilation-disable-input nil))
    (unless (boundp 'my-project-run-command-cache)
      (setq my-project-run-command-cache (make-hash-table :test 'equal)))
    (let* ((command (if compilation-read-command
                        (read-shell-command
                         "Run command: "
                         (gethash root my-project-run-command-cache))
                      (gethash root my-project-run-command-cache)))
           (buf (and command
                     (progn
                       (puthash root command my-project-run-command-cache)
                       ;; Run in comint-mode (interactive).
                       (compile command t))))))))

(defun my-project-compile-project (&optional prompt)
  "Compile the project.
With prefix argument PROMPT, always prompt for the compile command."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (default-directory root)
         (compilation-read-command
          (or prompt
              (not (and (boundp 'my-project-compile-command-cache)
			(gethash root my-project-compile-command-cache))))))
    (unless (boundp 'my-project-compile-command-cache)
      (setq my-project-compile-command-cache (make-hash-table :test 'equal)))
    (let ((command (if compilation-read-command
                       (read-shell-command "Compile command: "
                                           (or (gethash root my-project-compile-command-cache)
                                               compile-command))
                     (or (gethash root my-project-compile-command-cache)
                         compile-command))))
      (when command
        (puthash root command my-project-compile-command-cache)
        (compile command)))))

(defvar my/h-d-map (make-sparse-keymap)
  "My custom help submenu under C-h d.")
(define-key help-map (kbd "d") my/h-d-map)

(defvar my/c-o-map (make-sparse-keymap)
  "My custom open submenu under C-c o.")
(define-key global-map (kbd "C-c o") my/c-o-map)

(defvar my/c-t-map (make-sparse-keymap)
  "My custom toggle submenu under C-c t.")
(define-key global-map (kbd "C-c t") my/c-t-map)

(use-package emacs
  :ensure nil
  :bind
  (("C-c <left>" . winner-undo)
   ("C-c <right>" . winner-redo)
   ("C-x K" . kill-current-buffer)
   ("C-x O" . other-frame)
   ("C-|" . (lambda () (interactive)
              (duplicate-line)
              (forward-line)
              (doom/forward-to-last-non-comment-or-eol)))

   ;; remaps
   ([remap next-error] . flymake-goto-next-error)
   ([remap pre-error] . flymake-goto-prev-error)
   ([remap move-beginning-of-line] . doom/backward-to-bol-or-indent)
   ([remap move-end-of-line] . doom/forward-to-last-non-comment-or-eol)

   ;; unbind
   ("M-<tab>" . nil)
   ("C-M-i" . nil)

   ;; maps
   :map prog-mode-map
   ("C-c C-c" . my-project-compile-project)
   ("C-c C-v" . my-project-run-project)
   :map my/h-d-map
   ("c" . open-emacs-config)
   :map my/c-o-map
   ("t" . my-new-eshell)))

;; global modes
(which-key-mode t)
(electric-pair-mode t)
(winner-mode t)
(delete-selection-mode t)
(global-auto-revert-mode t)
(global-visual-line-mode t)
(global-so-long-mode nil)
(repeat-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key [remap list-buffers] 'ibuffer)
(setq eshell-banner-message "")
(setenv "DISPLAY" ":0")
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(setq window-sides-vertical t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; packages

(use-package transient)

(use-package magit
  :after transient
  :bind
  ("C-x g" . magit-status))

(use-package crux
  :bind
  (("M-o" . crux-smart-open-line-above)
   ("C-o" . crux-smart-open-line)))

(use-package beacon
  :custom
  (beacon-color "#ffff00")
  :config
  (beacon-mode t))

(use-package ace-window
  :bind
  (("C-x o" . ace-window)
   ("C-c C-o" . ace-swap-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package ctrlf
  :config
  (ctrlf-mode t))

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

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
				       if_statement with_statement while_statement)))
  (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)))
  :hook ((python-ts-mode yaml-mode zig-ts-mode) . indent-bars-mode))

(use-package apheleia
  :config
  (apheleia-global-mode t)
  (setf (alist-get 'python-mode apheleia-mode-alist)
	'(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
	'(ruff-isort ruff)))

(use-package eat
  :ensure (:type git :host codeberg :repo "akib/emacs-eat")
  :hook (eshell-load . eat-eshell-mode))

(use-package popper
  :bind
  (("C-`" . popper-toggle)
   ("C-<tab>" . popper-cycle))
  :custom
  (popper-group-function #'popper-group-by-project)
  :init
  (setq popper-reference-buffers
        '("\\*.*eshell.*\\*"))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (advice-add 'popper-close-latest :around #'popper--close-advice)
  (add-hook 'popper-open-popup-hook #'popper--open-hook))

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Welcome home")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :config
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(use-package quickrun
  :bind
  (("C-c r" . quickrun)))

(use-package yasnippet
  :bind
  (("C-c SPC" . yas-expand)
   :map yas-minor-mode-map
   ("<tab>" . nil))
  :config
  (yas-global-mode t))

(use-package yasnippet-capf
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package consult
  :bind
  (([remap goto-line] . consult-goto-line)
   ([remap switch-to-buffer] . consult-buffer)
   ("C-c s" . consult-ripgrep)
   ("C-c x" . consult-flymake)))

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
  :init
  (global-corfu-mode)
  :bind
  (("M-`" . completion-at-point)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package eglot
  :ensure nil
  :bind
  (:map eglot-mode-map
	("M-RET" . eglot-code-actions))
  :hook
  (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  (v-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (zig-ts-mode . eglot-ensure)
  (dart-ts-mode . eglot-ensure))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-org-config))

(use-package zen-mode
  :bind
  (:map my/c-t-map
	("z" . zen-mode)))

(use-package undo-fu
  :bind
  (([remap undo] . undo-fu-only-undo)
   ([remap undo-redo] . undo-fu-only-redo)))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode t))

(use-package vundo
  :bind
  (("C-x u" . vundo)))

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package posframe)
(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package transpose-frame
  :bind
  (("C-x M-o" . transpose-frame)))

(use-package gptel
  ;; :general
  ;; (cz-def "C-z" 'gptel-menu)
  ;; (cz-def "a" 'gptel-add)
  ;; (cz-def "f" 'gptel-add-file)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (gptel-highlight-mode)
  (setq gptel-model   'x-ai/grok-4.1-fast
        gptel-default-mode 'markdown-mode
	gptel-backend
	(gptel-make-openai "OpenRouter"
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
	  :key (lambda () (auth-source-pick-first-password :host "openrouter.ai" :user "apikey"))
          :models '(openai/gpt-4.1-nano
		    openai/gpt-4.1-mini
		    openai/gpt-4.1
		    openai/gpt-5-nano
                    openai/gpt-5-codex
                    openai/gpt-5
                    openai/gpt-5-mini
                    google/gemini-2.5-pro
                    anthropic/claude-sonnet-4.5
                    x-ai/grok-code-fast-1
                    x-ai/grok-4-fast
                    x-ai/grok-4.1-fast:free
                    x-ai/grok-4.1-fast
                    deepseek/deepseek-v3.2-exp
                    google/gemini-2.5-flash))))

;; (use-package gptel-quick
;;   :ensure (:type git :host github :repo "karthink/gptel-quick")
;;   :general
;;   (cz-def "?" 'gptel-quick))

(use-package gptel-commit
  :custom
  (gptel-commit-stream t)
  (gptel-commit-backend (gptel-make-openai "OpenRouterCommit"
                          :host "openrouter.ai"
                          :endpoint "/api/v1/chat/completions"
                          :stream t
	                  :key (lambda () (auth-source-pick-first-password :host "openrouter.ai" :user "apikey"))
                          :models '(x-ai/grok-4.1-fast)))
  :bind
  (:map git-commit-mode-map
	("C-c g" . gptel-commit)
	("C-c G" . gptel-commit-rationale)))

(use-package inheritenv)

;; (defun my-claude-notify (title message)
;;   "Display a Linux notification using notify-send."
;;   (if (executable-find "notify-send")
;;       (call-process "notify-send" nil nil nil title message)
;;     (message "%s: %s" title message)))

;; (use-package monet
;;   :ensure (:type git :host github :repo "stevemolitor/monet"))
;; (use-package claude-code
;;   :ensure (:type git :host github :repo "stevemolitor/claude-code.el")
;;   :custom
;;   (claude-code-notification-function #'my-claude-notify)
;;   :config
;;   (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
;;   (monet-mode 1)

;;   (claude-code-mode)
;;   :bind
;;   (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*claude:"
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . t)
;;                (pop-up-frame-parameters . ((name . "Claude Code")))))

;; (use-package vterm)


(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down))
  :config
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

;; Flutter packages

(use-package flutter
  :after dart-ts-mode)

;; Python packages

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-base-mode . ("ruff" "server"))))
(add-hook 'python-base-mode-hook
          (lambda ()
            (eglot-ensure)
            (add-hook 'after-save-hook 'eglot-format nil t)))

(use-package flymake-ruff
  :config
  (add-hook 'python-mode-hook #'flymake-ruff-load))

;; Replace default (black) to use ruff for sorting import and formatting.


(use-package jupyter)

;; Zig packages

(use-package zig-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/zig-ts-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode)))

;; (use-package zig-mode)

;; Dart packages

(use-package dart-ts-mode
  :ensure (:type git :host github :repo "50ways2sayhard/dart-ts-mode")
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(dart-ts-mode . ("dart" "language-server" "--client-id" "emacs.eglot-dart")))))

(add-to-list 'major-mode-remap-alist '(dart-mode . dart-ts-mode))

;; YAML packages

;; (use-package yaml-ts-mode
;;   :mode ("\\.yaml\\'" . yaml-ts-mode))

;; V packages

(use-package v-mode
  :ensure (:type git :host github :repo "elogir/v-mode")
  :mode ("\\(\\.v?v\\|\\.vsh\\)$" . 'v-mode)
  :init
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(v-mode . ("vls")))))

;; C packages

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

;; C++ packages

(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

;; Html mode

(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
(add-to-list 'major-mode-remap-alist '(mhtml-mode . html-ts-mode))

;; Typst packages

(use-package typst-ts-mode)

;; PHP packages

(use-package php-ts-mode
  :ensure (:type git :host github :repo "emacs-php/php-ts-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(php-ts-mode . ("intelephense" "--stdio")))))

(use-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.blade.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :config
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))))

(provide 'init)

(put 'erase-buffer 'disabled nil)

(setq gptel-commit-prompt
      "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

If you can accurately express the change in just the subject line, don't include anything in the message body. Only use the body when it is providing *useful* information.

Don't repeat information from the subject line in the message body.

Only return the commit message in your response. Do not include any additional meta-commentary about the task. Do not include the raw diff output in the commit message.

Follow good Git style:

- Separate the subject from the body with a blank line
- Try to limit the subject line to 50 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- Use the imperative mood in the subject line
- Wrap the body at 72 characters
- Keep the body short and concise (omit it entirely if not useful)")

(use-package org-roam
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("M-`" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (julia . t)
   (python . t)
   (jupyter . t)))

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))

;; call org-hide-properties after inserting a new node
(add-hook 'org-roam-post-node-insert-hook #'(lambda (_ _) (org-hide-properties)))

(use-package casual
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu)))

(defvar bazel-zig-last-target "//" 
  "Last Bazel target used for debugging.")

(defface my-blue-face
  '((((background dark)) :background "#293f56")
    (((background light)) :background "#d6c6a9"))  ; Light complement
  "Face that adapts to theme background.")

(use-package dape
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-request-timeout 30)
  :config
  (add-hook 'dape-display-source-hook 
            (lambda () 
              (pulse-momentary-highlight-one-line 
               (point) 
               'my-blue-face)))
  (add-to-list 'dape-configs
               `(bazel-zig
                 modes (zig-mode)
                 ensure dape-ensure-command
                 command-cwd dape-command-cwd
                 command ,(file-name-concat dape-adapter-dir
                                            "codelldb"
                                            "extension"
                                            "adapter"
                                            "codelldb")
                 command-args ("--port" :autoport "--settings"
                               "{\"sourceLanguages\":[\"zig\"],\"evaluationTimeout\":5,\"summaryTimeout\":1}")
                 port :autoport
                 target ,(lambda ()
                           (setq bazel-zig-last-target
                                 (read-string "Bazel target: " bazel-zig-last-target)))
                 fn ,(lambda (config)
                       (let* ((target (plist-get config 'target))
                              (compile-cmd (format "bazel build --compilation_mode=dbg --spawn_strategy=local %s" target))
                              (target-clean (string-trim-left target "//"))
                              (parts (split-string target-clean ":"))
                              (path (car parts))
                              (name (if (cdr parts) 
                                        (cadr parts) 
                                      (file-name-nondirectory path)))
                              (binary-path (expand-file-name 
                                            (format "bazel-bin/%s/%s" path name)
                                            (dape-cwd)))
                              (exec-root (string-trim
                                          (shell-command-to-string "bazel info execution_root"))))
                         (thread-first config
                                       (plist-put 'compile compile-cmd)
                                       (plist-put ':program binary-path)
                                       (plist-put 'prefix-remote (file-name-as-directory exec-root)))))
                 prefix-local ,(lambda () (file-name-as-directory (dape-cwd)))
                 :type "lldb"
                 :request "launch"
                 :cwd dape-cwd
                 :initCommands ["command source ~/.lldbinit"
                                "settings set target.max-children-count 50"
                                "settings set target.max-string-summary-length 200"]
                 :args []
                 :stopOnEntry nil)))

;; (use-package greader
;;   :ensure (:type git :host gitlab :repo "michelangelo-rodriguez/greader")
;;   :custom
;;   (greader-current-backend 'greader-speechd))

;; ;;; init.el ends here
