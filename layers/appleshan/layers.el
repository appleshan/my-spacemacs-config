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
  (appleshan-core
    :packages calendar mule recentf profiler whitespace ibuffer helpful
    ; desktop
    )
  appleshan-complete
  (appleshan-appearance
    :packages anzu all-the-icons on-screen paren-face popwin visual-regexp visual-regexp-steroids zoom
    ; beacon
    )
  (appleshan-navigation
    :packages dumb-jump evil evil-escape imenu-anywhere treemacs
    ; workgroups2
    )
  (appleshan-notify
    :packages alert sauron eshell)
  appleshan-shell
  (appleshan-dired
    :packages dired dired-x dired-k dired-quick-sort dired-efap quick-preview)
  (appleshan-org
    :variables org-gtd-dir (concat user-projects-directory "org-gtd/"))
  (appleshan-chinese
    :packages cal-china-x find-by-pinyin-dired ace-pinyin pangu-spacing
    ; pyim pyim-basedict pyim-greatdict
    :variables chinese-enable-fcitx nil)
  appleshan-programming
  (appleshan-python
    :packages company lsp-python ; py-autopep8
    )
  ; appleshan-java
  ; appleshan-javascript
  ; appleshan-web
  ; appleshan-lisp
  ; appleshan-database
  ; appleshan-remote
  appleshan-misc
  ))
