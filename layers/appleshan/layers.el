;;; layers.el --- Spacemacs Layer layers File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layers '(
  appleshan-core
  appleshan-complete
  (appleshan-appearance
    :packages all-the-icons 
    ; beacon
    hl-anything popwin visual-regexp visual-regexp-steroids
    ; tabbar
    )
  appleshan-navigation
  appleshan-notify
  ; appleshan-shell
  appleshan-dired
  (appleshan-org
    :variables org-gtd-dir (concat user-projects-directory "org-gtd/"))
  (appleshan-chinese
    :packages cal-china-x find-by-pinyin-dired ace-pinyin pangu-spacing
    :variables chinese-enable-fcitx nil)
  appleshan-programming
  appleshan-python
  ; appleshan-java
  ; appleshan-javascript
  ; appleshan-web
  appleshan-lisp
  ; appleshan-database
  ; appleshan-remote
  appleshan-misc
  ))
