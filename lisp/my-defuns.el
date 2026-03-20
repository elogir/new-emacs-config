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

;;; Org property drawers

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer."
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

(provide 'my-defuns)
;;; my-defuns.el ends here
