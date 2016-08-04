;;; funcs.el --- appleshan-chinese Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; {{ make IME compatible with evil-mode
(defun appleshan-chinese/evil-toggle-input-method ()
  "when toggle on input method, goto evil-insert-state. "
  (interactive)

  (cond
   ((and (boundp 'evil-mode) evil-mode)
    ;; evil-mode
    (cond
     ((eq evil-state 'insert)
      (toggle-input-method))
     (t
      (evil-insert-state)
      (unless current-input-method
        (toggle-input-method))
      ))
    (if current-input-method (message "IME on!")))
   (t
    ;; NOT evil-mode, some guy don't use evil-mode at all
    (toggle-input-method))))

(defadvice evil-insert-state (around evil-insert-state-hack activate)
  ad-do-it
  (if current-input-method (message "IME on!")))
;; }}

;; Universal Charset Auto Detector.
;; @see https://code.google.com/p/unicad/wiki/FAQ_Chinese
; (require 'unicad)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
