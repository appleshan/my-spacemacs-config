;;; config.el --- appleshan-python Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; fix bug : void-variable python-shell--interpreter
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(setq python-shell--interpreter "ipython"
      python-shell--interpreter-args "-i")

(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
