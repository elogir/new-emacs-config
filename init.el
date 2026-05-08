;;; init.el --- -*- lexical-binding: t; -*-

;;; Bootstrap

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'doom-text)
(require 'my-defuns)
(require 'package)

;;; Package setup

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

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

;;; Helpers

(defun my/disable-line-numbers ()
  "Disable `display-line-numbers-mode' in the current buffer."
  (display-line-numbers-mode -1))

(defun my/eshell-setup-company ()
  "Configure company backends for eshell."
  (setq-local company-backends '(esh-autosuggest))
  (setq-local company-idle-delay 0))

(defun my/savehist-clean-kill-ring ()
  "Strip text properties and non-strings from `kill-ring' before saving."
  (setq kill-ring
        (mapcar #'substring-no-properties
                (cl-remove-if-not #'stringp kill-ring))))

(defun my/recenter-after-save-place (&rest _)
  "Recenter point after `save-place' restores position."
  (when buffer-file-name (ignore-errors (recenter))))

(defun my-claude-display-right (buffer)
  "Display Claude buffer in a regular window on the right."
  (display-buffer buffer '((display-buffer-in-direction)
                           (direction . right)
                           (window-width . 90))))

;;; Built-in configuration

(use-package emacs
  :ensure nil
  :custom
  (use-short-answers t)
  (isearch-lazy-count t)
  (display-line-numbers-type 'relative)
  (window-sides-vertical t)
  (save-interprogram-paste-before-kill t)
  (help-window-select t)
  (reb-re-syntax 'string)
  (ffap-machine-p-known 'reject)
  (redisplay-skip-fontification-on-input t)
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
   ([remap list-buffers] . ibuffer)
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
   ("t" . ghostel))
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  :config
  (which-key-mode 1)
  (electric-pair-mode 1)
  (winner-mode 1)
  (delete-selection-mode 1)
  (global-auto-revert-mode 1)
  (global-visual-line-mode 1)
  (global-so-long-mode 1)
  (xterm-mouse-mode 1)
  (repeat-mode 1)
  (global-completion-preview-mode 0)
  (menu-bar-mode -1)
  (global-display-line-numbers-mode 1)

  ;; Non-defcustom variables: must use setq.
  (setq read-process-output-max (* 4 1024 1024)
        eshell-banner-message ""
        bidi-inhibit-bpa t)
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  (setenv "DISPLAY" ":0")
  (put 'erase-buffer 'disabled nil))

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1)
  :config (advice-add 'save-place-find-file-hook :after
                      #'my/recenter-after-save-place))

(use-package savehist
  :ensure nil
  :hook (savehist-save . my/savehist-clean-kill-ring))

(use-package tramp
  :ensure nil
  :custom
  (tramp-verbose 1)
  (tramp-auto-save-directory "~/tmp/tramp-autosave/")
  :config
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

;;; Packages — completion

(use-package consult
  :ensure t
  :bind
  (([remap goto-line] . consult-goto-line)
   ([remap switch-to-buffer] . consult-buffer)
   ("C-c s" . consult-ripgrep)
   ("C-c x" . consult-flymake)))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :bind ("M-`" . completion-at-point))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package yasnippet
  :ensure t
  :bind
  (("C-c SPC" . yas-expand)
   :map yas-minor-mode-map
   ("<tab>" . nil))
  :config (yas-global-mode 1))

(use-package yasnippet-capf
  :ensure t
  :after yasnippet
  :config (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;; Packages — UI

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-org-config))

(use-package beacon
  :ensure t
  :custom (beacon-color "#ffff00")
  :config
  (beacon-mode 1)
  (add-to-list 'beacon-dont-blink-predicates
               (lambda () (string-match-p "\\`\\*\\(ghostel:\\|claude:\\)" (buffer-name)))))

(use-package nerd-icons :ensure t)

(use-package colorful-mode
  :ensure t
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode 1)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package indent-bars
  :ensure t
  :custom
  (indent-bars-prefer-character t)
  (indent-bars-no-stipple-char ?│)
  (indent-bars-color '("gray50" :blend 0.3))
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:blend 0.5))
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  (indent-bars-treesit-wrap '((c argument_list parameter_list init_declarator parenthesized_expression)))
  :hook ((python-ts-mode yaml-mode zig-ts-mode) . indent-bars-mode))

(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-mode 1))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title "Welcome home")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  :hook ((after-init . dashboard-insert-startupify-lists)
         (after-init . dashboard-initialize))
  :config (dashboard-setup-startup-hook))

(use-package ultra-scroll
  :ensure t
  :config (ultra-scroll-mode t))

(use-package winpulse
  :ensure t
  :vc (:url "https://github.com/xenodium/winpulse" :rev :newest)
  :custom (winpulse-brightness 3)
  :config (winpulse-mode +1))

;;; Packages — editing

(use-package crux
  :ensure t
  :bind
  (("M-o" . crux-smart-open-line-above)
   ("C-o" . crux-smart-open-line)
   ("C-x C-o" . crux-transpose-windows)))

(use-package undo-fu
  :ensure t
  :bind
  (([remap undo] . undo-fu-only-undo)
   ([remap undo-redo] . undo-fu-only-redo)))

(use-package undo-fu-session
  :ensure t
  :config (undo-fu-session-global-mode 1))

(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

(use-package move-text
  :ensure t
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down))
  :config
  (advice-add 'move-text-up :after #'indent-region-advice)
  (advice-add 'move-text-down :after #'indent-region-advice))

(use-package noccur :ensure t)

;;; Packages — git

(use-package project
  :ensure nil
  :custom
  (project-switch-commands
   '((project-find-file "Find file" "f")
     (project-dired "Dired" "d")
     (ghostel-project "Terminal" "t")
     (magit-project-status "Magit" "m"))))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package gptel-commit
  :ensure t
  :after git-commit
  :custom
  (gptel-commit-use-claude-code t)
  (gptel-commit-prompt
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
- Keep the body short and concise (omit it entirely if not useful)

Also follow previous commits style
Never mention that it's written by an AI, nor mention Claude
")
  :bind (:map git-commit-mode-map
              ("C-c g" . gptel-commit)
              ("C-c G" . gptel-commit-rationale)))

;;; Packages — terminal & windows

(use-package spatial-window
  :ensure t
  :vc (:url "https://github.com/lewang/spatial-window")
  :custom (spatial-window-overlay-delay 3)
  :bind ("C-x o" . spatial-window-toggle-or-select))

(use-package transpose-frame
  :ensure t
  :bind ("C-x M-o" . transpose-frame))

(use-package sway
  :ensure t
  :vc (:url "https://github.com/thblt/sway.el" :rev :newest)
  :if (and (featurep 'pgtk) (getenv "SWAYSOCK"))
  :custom
  (frame-title-format
   '("%b — GNU Emacs ["
     (:eval (frame-parameter (selected-frame) 'window-id))
     "]"))
  :config
  (sway-socket-tracker-mode)
  (sway-x-focus-through-sway-mode))

(use-package kkp
  :ensure t
  :hook (tty-setup . global-kkp-mode))

(use-package ghostel
  :ensure t
  :vc (:url "https://github.com/dakra/ghostel"
            :lisp-dir "lisp"
            :rev :newest)
  :hook (ghostel-mode . my/disable-line-numbers)
  :preface
  (defun my/ghostel-readonly-copy-stay ()
    "Copy the active region in read-only mode without exiting the mode."
    (interactive)
    (if (use-region-p)
        (let ((text (ghostel--clean-copy-text
                     (buffer-substring (region-beginning) (region-end)))))
          (kill-new text)
          (deactivate-mark)
          (message "Copied to kill ring"))
      (message "No region selected")))
  (defun my/ghostel-readonly-paste-and-exit ()
    "Exit read-only mode and paste the most recent kill into the terminal."
    (interactive)
    (ghostel-readonly-exit)
    (ghostel-yank))
  :bind (:map ghostel-readonly-mode-map
              ("M-w" . my/ghostel-readonly-copy-stay)
              ("C-w" . my/ghostel-readonly-copy-stay)
              ("C-y" . my/ghostel-readonly-paste-and-exit)))

;;; Packages — eshell

(use-package eshell
  :ensure nil
  :custom
  (eshell-history-append t)
  (eshell-prompt-function #'my/eshell-prompt)
  (eshell-prompt-regexp "^\\(?:(.*) \\)?.*[$#] ")
  :bind (:map eshell-mode-map
              ("C-l" . aweshell-clear-buffer))
  :config
  (advice-add 'epe-git-p :override (lambda () nil))
  (advice-add 'eshell/cat :override #'aweshell-cat-with-syntax-highlight)
  (defalias 'eshell/v 'eshell-exec-visual)
  (defalias 'eshell/e 'aweshell-emacs)
  ;; Run last (depth 90) to override anything aweshell set.
  (add-hook 'eshell-mode-hook #'my/eshell-setup-company 90))

(use-package em-hist
  :ensure nil
  :bind (:map eshell-hist-mode-map
              ("M-r" . eshell-atuin-history)))

(use-package esh-autosuggest
  :ensure t
  :hook (eshell-mode . esh-autosuggest-mode)
  :bind (:map esh-autosuggest-active-map
              ("C-e" . company-complete-selection)))

(use-package eshell-atuin
  :ensure t
  :vc (:url "https://github.com/elogir/eshell-atuin.git" :rev :newest)
  :after eshell
  :custom (eshell-atuin-filter-mode 'session-preload)
  :config (eshell-atuin-mode))

(use-package eshell-syntax-highlighting
  :ensure t
  :config (eshell-syntax-highlighting-global-mode +1))

;;; Packages — Claude Code

(use-package inheritenv :ensure t)

(use-package monet
  :ensure t
  :demand t
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/elogir/claude-code.el.git" :rev :newest)
  :after (monet inheritenv)
  :custom
  (claude-code-display-buffer-on-send nil)
  (claude-code-terminal-backend 'ghostel)
  (claude-code-term-name "xterm-ghostty")
  (claude-code-display-window-fn #'my-claude-display-right)
  (claude-code-voice-auto-send 'delay)
  :bind-keymap ("C-M-c" . claude-code-command-map)
  :bind (("M-z" . claude-code-send-command)
         ("<f12>" . claude-code-voice-hold))
  :config
  (add-hook 'claude-code-process-environment-functions
            #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode 1))

;;; Packages — misc

(use-package posframe :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

(use-package jupyter
  :ensure t
  :custom (jupyter-repl-echo-eval-p t))

;;; Languages

(require 'my-langs)

;; init.el ends here
