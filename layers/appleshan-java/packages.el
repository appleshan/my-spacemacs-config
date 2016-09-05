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
      ; (flycheck-java :location local)
      (flycheck-infer :location local)
      javadoc-lookup
      mvn
      ))

;; List of packages to exclude.
(setq appleshan-java-excluded-packages '())

(defun appleshan-java/init-ajoke ()
  (use-package ajoke
    :load-path "/home/apple/repo/git/java/ajoke/etc/elisp/"
    :defer t
    ; :init (require 'ajoke)
    ))

; (defun appleshan-java/init-flycheck-java ()
;   (use-package flycheck-java
;     :defer t
;     :init
;     (progn
;       (add-hook 'java-mode-hook
;                 (lambda ()
;                   (setq flycheck-java-ecj-jar-path "/home/apple/bin/develop/java/ecj-4.5.1.jar")
;                   (require 'flycheck-java)
;                   ))
;       )
;     ))

(defun appleshan-java/init-flycheck-infer ()
  (use-package flycheck-infer
    :defer t
    :init
    (progn
      (add-hook 'java-mode-hook
                (lambda ()
                  (require 'flycheck-infer)
                  ))
      )
    ))

(defun appleshan-java/init-javadoc-lookup ()
  (use-package javadoc-lookup
    :config
    (progn
      ; (javadoc-add-roots "/usr/share/doc/openjdk-8-jdk/api")
      ; (javadoc-add-artifacts
      ;   [commons-lang commons-lang "2.6"]
      ;   [com.alibaba dubbo "2.8.4"]
      ;   [com.daanhealth ncl-services-api "1.0.1-SNAPSHOT"])
      )))

(defun appleshan-java/init-mvn ()
  (use-package mvn))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
