;;; packages.el --- appleshan-web Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq appleshan-web-packages
    '(
      css-mode
      impatient-mode
      ; web-mode
      ))

;; List of packages to exclude.
(setq appleshan-web-excluded-packages '())

(defun appleshan-web/post-init-css-mode ()
  (with-eval-after-load 'css-mode
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))
    ))

(defun appleshan-web/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn
      (add-hook 'web-mode-hook 'appleshan/impatient-mode-hook)
      (spacemacs/set-leader-keys-for-major-mode 'web-mode
        "p" 'imp-visit-buffer)
      )))

; (defun appleshan-web/post-init-web-mode ()
;   (setq company-backends-web-mode '((company-dabbrev-code
;                                      company-keywords
;                                      company-etags)
;                                     company-files company-dabbrev)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
