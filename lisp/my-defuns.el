;;; my-defuns.el --- Custom functions -*- lexical-binding: t; -*-

;;; Popper size persistence

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

;;; Navigation & editing

(defun open-emacs-config ()
  "Open the Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun my-project-run-project (&optional prompt)
  "Run a command in the project root directory.
With prefix argument PROMPT, always prompt for the command."
  (interactive "P")
  (let* ((pr (project-current t))
         (root (project-root pr))
         (default-directory root)
         (compilation-read-command
          (or prompt
              (not (and (boundp 'my-project-run-command-cache)
                        (gethash root my-project-run-command-cache)))))
         (compilation-disable-input nil))
    (unless (boundp 'my-project-run-command-cache)
      (setq my-project-run-command-cache (make-hash-table :test 'equal)))
    (let ((command (if compilation-read-command
                       (read-shell-command
                        "Run command: "
                        (gethash root my-project-run-command-cache))
                     (gethash root my-project-run-command-cache))))
      (when command
        (puthash root command my-project-run-command-cache)
        (compile command t)))))

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

(defun indent-region-advice (&rest _ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(defun my/maybe-eglot ()
  "Start eglot only for local files."
  (unless (file-remote-p default-directory)
    (eglot-ensure)))

(defun spatial-window-toggle-or-select ()
  "Use `other-window' with 2 windows, `spatial-window-select' otherwise."
  (interactive)
  (if (= (count-windows) 2)
      (other-window 1)
    (spatial-window-select)))

(defun agent-shell-prompt-minibuffer (&optional pick-shell)
  "Read a prompt from the minibuffer and send it to an `agent-shell'.

If a region is active, embed it as context.

With \\[universal-argument] prefix PICK-SHELL, prompt for which shell to use."
  (interactive "P")
  (let* ((shell-buffer (if pick-shell
                           (let* ((buffers (agent-shell-buffers))
                                  (start-new "Start new shell")
                                  (choices (if buffers
                                               (append (mapcar #'buffer-name buffers)
                                                       (list start-new))
                                             (list start-new)))
                                  (choice (completing-read "Send to shell: "
                                                           choices nil t)))
                             (if (equal choice start-new)
                                 (agent-shell--start
                                  :config (or (agent-shell--resolve-preferred-config)
                                              (agent-shell-select-config
                                               :prompt "Start new agent: ")
                                              (error "No agent config found"))
                                  :no-focus t
                                  :new-session t
                                  :session-strategy 'new)
                               (get-buffer choice)))
                         ;; Use 'new to avoid nested minibuffer from
                         ;; session strategy 'prompt timer while still
                         ;; eagerly initializing the session.
                         (let ((agent-shell-session-strategy 'new))
                           (agent-shell--shell-buffer))))
         (context (when (region-active-p)
                    (agent-shell--get-region-context
                     :deactivate t
                     :agent-cwd (with-current-buffer shell-buffer
                                  (agent-shell-cwd)))))
         (prompt (read-string "Prompt: ")))
    (when (string-empty-p prompt)
      (user-error "No prompt provided"))
    (with-current-buffer shell-buffer
      (shell-maker-clear-buffer))
    (agent-shell-insert
     :text (if context
               (concat prompt "\n\n" context)
             prompt)
     :submit t
     :no-focus t
     :shell-buffer shell-buffer)
    (display-buffer shell-buffer
                    '((display-buffer-in-direction)
                      (direction . right)))))

(defun aweshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; Make cat with syntax highlight.
(defun aweshell-cat-with-syntax-highlight (filename)
  "Like cat(1) but with syntax highlighting."
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (let ((contents (buffer-string)))
         (remove-text-properties 0 (length contents) '(read-only nil) contents)
         contents)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

(defun aweshell-emacs (&rest args)
  "Open a file in Emacs with ARGS, Some habits die hard."
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else naming a directory in an
    ;; argument causes later arguments to be looked for in that directory,
    ;; not the starting directory
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defvar-local my/eshell-old-path nil)

(defun eshell/venv (&optional dir)
  "Activate or deactivate a virtualenv in eshell."
  (if dir
      (let* ((venv-dir (file-name-as-directory (expand-file-name dir)))
             (local-dir (file-local-name venv-dir))
             (local-bin (concat local-dir "bin")))
        (eshell/venv) ;; deactivate first
        (setq my/eshell-old-path (eshell-command-result "echo $PATH"))
        (eshell-command-result (format "export VIRTUAL_ENV=%s" (shell-quote-argument local-dir)))
        (eshell-command-result (format "export PATH=%s:$PATH" (shell-quote-argument local-bin)))
        (message "Activated: %s" local-dir))
    (when my/eshell-old-path
      (eshell-command-result (format "export PATH=%s" (shell-quote-argument my/eshell-old-path)))
      (eshell-command-result "unset VIRTUAL_ENV")
      (setq my/eshell-old-path nil)
      (message "Deactivated venv"))))

(defun my/eshell-prompt ()
  "Custom eshell prompt showing venv and current directory."
  (let ((venv (getenv "VIRTUAL_ENV")))
    (concat
     (when venv
       (propertize (format "(%s) "
                           (file-name-nondirectory (directory-file-name venv)))
                   'face '(:foreground "cyan")))
     (propertize (abbreviate-file-name (eshell/pwd))
                 'face '(:foreground "gold"))
     (if (= (user-uid) 0) " # " " $ "))))

(provide 'my-defuns)
;;; my-defuns.el ends here
