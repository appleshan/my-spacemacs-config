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
  appleshan-appearance
  appleshan-navigation
  appleshan-notify
  appleshan-shell
  appleshan-dired
  appleshan-org
  (appleshan-chinese :variables chinese-enable-fcitx t)
  appleshan-programming
  appleshan-python
  appleshan-java
  ; appleshan-javascript
  ; appleshan-web
  ; appleshan-lisp
  ; appleshan-database
  ; appleshan-remote
  appleshan-misc
  ))
