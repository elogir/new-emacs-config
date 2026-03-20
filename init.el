;;; init.el --- -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'doom-text)
(require 'my-defuns)

;;; Package setup
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t)

;;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; Backups & autosave
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;; Keymaps

(defvar my/h-d-map (make-sparse-keymap) "Custom help submenu under C-h d.")
(define-key help-map (kbd "d") my/h-d-map)

(defvar my/c-o-map (make-sparse-keymap) "Custom open submenu under C-c o.")
(define-key global-map (kbd "C-c o") my/c-o-map)

(defvar my/c-t-map (make-sparse-keymap) "Custom toggle submenu under C-c t.")
(define-key global-map (kbd "C-c t") my/c-t-map)

;;; Built-in configuration

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
   ([remap imenu] . consult-imenu)
   ("M-g M-i" . consult-imenu-multi)
   ([remap next-error] . flymake-goto-next-error)
   ([remap previous-error] . flymake-goto-prev-error)
   ("C-a" . doom/backward-to-bol-or-indent)
   ("C-e" . doom/forward-to-last-non-comment-or-eol)
   ([remap move-beginning-of-line] . doom/backward-to-bol-or-indent)
   ([remap beginning-of-visual-line] . doom/backward-to-bol-or-indent)
   ([remap end-of-visual-line] . doom/forward-to-last-non-comment-or-eol)
   ([remap move-end-of-line] . doom/forward-to-last-non-comment-or-eol)
   ("M-<tab>" . nil)
   ("C-M-i" . nil)
   ("C-z" . nil)
   :map prog-mode-map
   ("C-c C-c" . my-project-compile-project)
   ("C-c C-v" . my-project-run-project)
   :map my/h-d-map
   ("c" . open-emacs-config)
   :map my/c-o-map
   ("t" . multi-vterm)))

;;; Global modes
(which-key-mode 1)
(electric-pair-mode 1)
(winner-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(global-visual-line-mode 1)
(global-so-long-mode 1)
(xterm-mouse-mode 1)
(repeat-mode 1)
(global-completion-preview-mode 1)
(menu-bar-mode -1)

;;; Global settings
(setopt use-short-answers t)
(global-set-key [remap list-buffers] 'ibuffer)
(setq eshell-banner-message ""
      display-line-numbers-type 'relative
      window-sides-vertical t
      read-process-output-max (* 1024 1024))
(global-display-line-numbers-mode 1)
(setenv "DISPLAY" ":0")

;;; Packages — completion

(use-package consult
  :bind
  (([remap goto-line] . consult-goto-line)
   ([remap switch-to-buffer] . consult-buffer)
   ("C-c s" . consult-ripgrep)
   ("C-c x" . consult-flymake)))

(use-package vertico
  :init (vertico-mode))

(use-package marginalia
  :init (marginalia-mode))

(use-package corfu
  :init (global-corfu-mode)
  :bind ("M-`" . completion-at-point))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package yasnippet
  :bind
  (("C-c SPC" . yas-expand)
   :map yas-minor-mode-map
   ("<tab>" . nil))
  :config (yas-global-mode 1))

(use-package yasnippet-capf
  :after yasnippet
  :config (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;; Packages — UI

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-org-config))

(use-package beacon
  :custom (beacon-color "#ffff00")
  :config (beacon-mode 1))

(use-package emojify
  :custom (global-emojify-mode t))

(use-package nerd-icons)

(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode 1)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)))
  :hook ((python-ts-mode yaml-mode zig-ts-mode) . indent-bars-mode))

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-mode 1))

(use-package zen-mode
  :bind (:map my/c-t-map ("z" . zen-mode)))

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Welcome home")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :config
  (add-hook 'after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

;;; Packages — editing

(use-package crux
  :bind
  (("M-o" . crux-smart-open-line-above)
   ("C-o" . crux-smart-open-line)
   ("C-x C-o" . crux-transpose-windows)))

(use-package undo-fu
  :bind
  (([remap undo] . undo-fu-only-undo)
   ([remap undo-redo] . undo-fu-only-redo)))

(use-package undo-fu-session
  :config (undo-fu-session-global-mode 1))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down))
  :config
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(use-package noccur)

;;; Packages — git

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (with-eval-after-load 'project
    (setq project-switch-commands
          '((project-find-file "Find file" "f")
            (project-dired "Dired" "d")
            (magit-project-status "Magit" "m")))))

(use-package gptel-commit
  :custom (gptel-commit-use-claude-code t)
  :bind (:map git-commit-mode-map
              ("C-c g" . gptel-commit)
              ("C-c G" . gptel-commit-rationale))
  :init
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
- Keep the body short and concise (omit it entirely if not useful)"))

;;; Packages — terminal & windows

(use-package spatial-window
  :vc (:url "https://github.com/lewang/spatial-window")
  :bind ("C-x o" . spatial-window-toggle-or-select)
  :custom (spatial-window-expert-mode t))

(use-package vterm)

(use-package multi-vterm
  :custom (multi-vterm-dedicated-window-height-percent 30))

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat")
  :hook (eshell-load . eat-eshell-mode))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :branch "anti-flicker-fixes")
  :bind ("C-M-c" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'vterm)
  (advice-add 'claude-code-ide--sync-terminal-dimensions :around
              (lambda (orig-fun buffer window)
                (when (and buffer window (buffer-live-p buffer) (window-live-p window))
                  (with-current-buffer buffer
                    (when-let ((proc (get-buffer-process buffer)))
                      (let ((new-h (window-body-height window))
                            (new-w (window-body-width window))
                            (cur-h (process-get proc 'my/last-height))
                            (cur-w (process-get proc 'my/last-width)))
                        (unless (and (eql new-h cur-h) (eql new-w cur-w))
                          (process-put proc 'my/last-height new-h)
                          (process-put proc 'my/last-width new-w)
                          (funcall orig-fun buffer window)))))))))

(use-package popper
  :bind
  (("C-`" . popper-toggle)
   ("C-<tab>" . popper-cycle))
  :custom
  (popper-group-function #'popper-group-by-project)
  :init
  (setq popper-reference-buffers '("\\*.*vterminal.*\\*"))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (advice-add 'popper-close-latest :around #'popper--close-advice)
  (add-hook 'popper-open-popup-hook #'popper--open-hook))

;; (use-package gterm
;;   :vc (:url "https://github.com/rwc9u/emacs-libgterm")
;;   :custom
;;   (gterm-shell "/opt/homebrew/bin/bash")
;;   (gterm-term-environment-variable "xterm-256color"))

(add-hook 'vterm-mode-hook
          (lambda ()
            (set (make-local-variable 'buffer-face-mode-face)
                 '(:family "Meslo LG M"))
            (buffer-face-mode t)))

(use-package kkp
  :hook (tty-setup . global-kkp-mode))

;;; Packages — misc

(use-package posframe)
(use-package transpose-frame :bind ("C-x M-o" . transpose-frame))
(use-package inheritenv)
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package jupyter
  :custom (jupyter-repl-echo-eval-p t))

;;; TRAMP
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1
      tramp-auto-save-directory "~/tmp/tramp-autosave/")

;;; Languages
(require 'my-langs)

(put 'erase-buffer 'disabled nil)

;;; init.el ends here
