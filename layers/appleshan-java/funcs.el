;;; funcs.el --- appleshan-lisp Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun mvn-compile-full ()
  (interactive)
  (mvn "dependency:sources"))

; mvn package
(defun mvn-package ()
  (interactive)
  (mvn "package"))

; mvn install
(defun mvn-install ()
  (interactive)
  (mvn "install"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
