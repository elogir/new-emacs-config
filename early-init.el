;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Garbage collection — relax during startup, restore after init

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun my/gc-after-focus-change ()
  "Run GC when the frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun my/restore-gc-defaults ()
  "Restore sane GC values once startup is done."
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq gc-cons-threshold (* 100 1024 1024)
           gc-cons-percentage 0.1)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function
                     #'my/gc-after-focus-change)))))

(add-hook 'after-init-hook #'my/restore-gc-defaults)

;;; Native compilation

(setq native-comp-async-report-warnings-errors nil)

;;; Frame & UI

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t
      server-client-instructions nil
      ring-bell-function #'ignore
      inhibit-startup-screen t)

(set-face-font 'default "Adwaita Mono-10")

;; Skip applying X session resources (we manage faces ourselves).
(advice-add #'x-apply-session-resources :override #'ignore)

;;; early-init.el ends here
