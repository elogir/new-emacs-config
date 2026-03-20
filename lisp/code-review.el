;;; code-review.el --- Simple code review session notes -*- lexical-binding: t; -*-

;; Author: Raphael
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, org

;;; Commentary:

;; Start a review session, select code regions, annotate them,
;; and everything gets saved to an org file.

;;; Code:

(defvar my/review-file nil "Current review session org file.")

(defun my/review--strip-indent (code)
  "Remove common leading whitespace from CODE."
  (let* ((lines (split-string code "\n"))
         (non-empty (seq-filter (lambda (l) (not (string-empty-p (string-trim l)))) lines))
         (min-indent (if non-empty
                         (apply #'min (mapcar (lambda (l)
						(- (length l) (length (string-trim-left l))))
                                              non-empty))
                       0)))
    (mapconcat (lambda (l)
                 (if (> (length l) min-indent)
                     (substring l min-indent)
                   (string-trim l)))
               lines "\n")))

(defun my/review-start (file)
  "Start a code review session, saving notes to FILE."
  (interactive "FReview org file: ")
  (setq my/review-file (expand-file-name file))
  (with-current-buffer (find-file-noselect my/review-file)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert (format "* Review Session\n:PROPERTIES:\n:DATE: %s\n:END:\n"
                    (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (save-buffer))
  (message "Review session started → %s" my/review-file))

(defun my/review-comment (start end)
  "Capture selected region with a comment to the current review session."
  (interactive "r")
  (let* ((code (my/review--strip-indent
                (buffer-substring-no-properties start end)))
         (file (or (buffer-file-name) (buffer-name)))
         (line-start (line-number-at-pos start))
         (line-end (line-number-at-pos end))
         (mode-name (downcase
                     (replace-regexp-in-string "-\\(ts-\\)?mode$" ""
					       (symbol-name major-mode))))
         (comment (read-string "Note: ")))
    (message "DEBUG: major-mode=%s mode-name=%s" major-mode mode-name)
    (unless my/review-file
      (call-interactively #'my/review-start))
    (with-current-buffer (find-file-noselect my/review-file)
      (goto-char (point-max))
      (insert (format "\n** %s\n~%s~ Lines %d–%d\n\n#+begin_src %s\n%s\n#+end_src\n\n"
                      comment file line-start line-end mode-name code))
      (save-buffer))
    (deactivate-mark)
    (message "Noted: %s" comment)))

(defun my/review-end ()
  "End the current review session."
  (interactive)
  (if my/review-file
      (progn
        (message "Review session ended. Notes in %s" my/review-file)
        (setq my/review-file nil))
    (message "No active review session.")))

(provide 'code-review)
;;; code-review.el ends here
