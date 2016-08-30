;;; packages.el --- appleshan-java Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq appleshan-java-packages
    '(
      (ajoke :location local)
      ))

;; List of packages to exclude.
(setq appleshan-java-excluded-packages '())

(defun appleshan-java/init-ajoke ()
  (use-package ajoke
  	:load-path "/home/apple/git/java/ajoke/etc/elisp/"
    :defer t
    :init (require 'ajoke)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
