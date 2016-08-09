;;; config.el --- appleshan-misc Layer configuration File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar my-gmail-bbdb-file nil)

;; 联系人的数据库文件
(setq my-gmail-bbdb-file (concat user-dropbox-directory "apple-data/contacts/bbdb"))

(with-eval-after-load 'paradox
  (setq paradox-column-width-package 35
        paradox-column-width-version 16
        paradox-column-width-status  12
        paradox-column-width-star    6))

(define-abbrev-table 'global-abbrev-table
  '(
    ;; math/unicode symbols
    ("8in" "∈")
    ("8nin" "∉")
    ("8inf" "∞")
    ("8luv" "♥")
    ("8smly" "☺")
    ("8en" "@~english")
    ("8zh" "@~chinese")
    ("8sp" "spacemacs")

    ;; computing tech
    ("8wp" "Wikipedia")
    ("8ms" "Microsoft")
    ("8g" "Google")
    ("8msw" "Microsoft Windows")
    ("8win" "Windows")
    ("8ie" "Internet Explorer")

    ;; email
    ("8as" "apple.shan@gmail.com")
    ;; signature
    ("8me" "appleshan")

    ;; emacs regex
    ("8d" "\\([0-9]+?\\)")
    ("8str" "\\([^\"]+?\\)\"")))

;; 绑定扩展名到特定的模式
(dolist (elt-cons '(
                    ("\\.myclirc\\'" . conf-mode)
                    ("\\.lrc\\'" . emms-lyrics-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)))
  (add-to-list 'auto-mode-alist elt-cons))

;; Marking the *Messages* buffer as useful
(push "\\*Messages\\*" spacemacs-useful-buffers-regexp)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; config.el ends here
