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

(defun my-setup-prog-keybindings ()
  "Set up keybindings for programming modes."
  (general-define-key
   :keymaps 'local
   "C-c C-c" 'my-project-compile-project
   "C-c C-v" 'my-project-run-project))

(add-hook 'prog-mode-hook #'my-setup-prog-keybindings)

;; global modes
(which-key-mode t)
(electric-pair-mode t)
(winner-mode t)
(delete-selection-mode t)
(global-auto-revert-mode t)
(global-visual-line-mode t)
(global-so-long-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key [remap list-buffers] 'ibuffer)
(setq eshell-banner-message "")
(setenv "DISPLAY" ":0")
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
    [remap next-error] 'flymake-goto-next-error
    [remap prev-error] 'flymake-goto-prev-error
    [remap move-beginning-of-line] 'doom/backward-to-bol-or-indent
    [remap move-end-of-line] 'doom/forward-to-last-non-comment-or-eol
    "M-<tab>" nil
    "C-M-i" nil
    "C-x O" 'other-frame
    "C-|" (lambda () (interactive)
            (duplicate-line)
            (forward-line)
            (doom/forward-to-last-non-comment-or-eol)))
  (general-create-definer cz-def ; gptel prefix
    :prefix "C-z")
  (general-create-definer cc-def ; Comp prefix
    :prefix "C-c")
  (cc-def
    "<left>" 'winner-undo
    "<right>" 'winner-redo
    "o t" 'my-new-eshell)
  (general-create-definer cx-def ; Main prefix
    :prefix "C-x")
  (cx-def "K" 'kill-current-buffer)
  (general-create-definer ch-def ; Help prefix
    :prefix "C-h")
  (ch-def "d c" 'open-emacs-config))

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

(use-package beacon
  :custom
  (beacon-color "#ffff00")
  :config
  (beacon-mode t))

(use-package ace-window
  :general
  (cx-def "o" 'ace-window)
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
  :init
  (global-corfu-mode)
  :general
  (general-def
    "M-`" 'completion-at-point))

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
  :general
  (general-define-key :keymaps 'eglot-mode-map "M-RET" #'eglot-code-actions)
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
  :general
  (cc-def "t z" 'zen-mode))

(use-package undo-fu
  :general
  (general-def
    [remap undo] 'undo-fu-only-undo
    [remap undo-redo] 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode t))

(use-package vundo
  :general
  (cx-def "u" 'vundo))

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
  :general
  (cx-def "M-o" 'transpose-frame))

(use-package gptel
  :general
  (cz-def "C-z" 'gptel-menu)
  (cz-def "a" 'gptel-add)
  (cz-def "f" 'gptel-add-file)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (gptel-highlight-mode)
  (setq gptel-model   'x-ai/grok-4-fast
        gptel-default-mode 'org-mode
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
                    deepseek/deepseek-v3.2-exp
                    google/gemini-2.5-flash))))

(use-package gptel-quick
  :ensure (:type git :host github :repo "karthink/gptel-quick")
  :general
  (cz-def "?" 'gptel-quick))

(use-package gptel-commit
  :custom
  (gptel-commit-stream t)
  (gptel-commit-backend (gptel-make-openai "OpenRouterCommit"
                          :host "openrouter.ai"
                          :endpoint "/api/v1/chat/completions"
                          :stream t
	                  :key (lambda () (auth-source-pick-first-password :host "openrouter.ai" :user "apikey"))
                          :models '(x-ai/grok-4-fast)))
  :config
  (with-eval-after-load 'magit
    (define-key git-commit-mode-map (kbd "C-c g") #'gptel-commit)
    (define-key git-commit-mode-map (kbd "C-c G") #'gptel-commit-rationale)))

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
;;   :bind-keymap ("C-z" . claude-code-command-map)
;;   :bind
;;   (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*claude:"
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . t)
;;                (pop-up-frame-parameters . ((name . "Claude Code")))))

(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(use-package move-text
  :general
  (general-def
    "M-p" 'move-text-up
    "M-n" 'move-text-down)
  :config
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(use-package rg
  :config
  (rg-enable-menu))

;; Flutter packages

(use-package flutter
  :after dart-ts-mode
  :hook (dart-ts-mode . (lambda ()
                          (general-define-key :keymaps 'local
                                              "C-c C-c" #'flutter-run-or-hot-reload
                                              "C-c C-v" #'flutter-hot-restart))))

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

;;; init.el ends here
