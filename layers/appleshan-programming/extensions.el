;;; extensions.el --- appleshan-programming Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Pre extensions are loaded *before* the packages
(setq appleshan-programming-pre-extensions '())

;; Post extensions are loaded *after* the packages
(setq appleshan-programming-post-extensions
  '(
    vdiff
    ))

(defun appleshan-programming/init-vdiff ()
  (use-package vdiff
  	:defer t
    :commands (vdiff-buffers vdiff-files)
    :config
    (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)))
