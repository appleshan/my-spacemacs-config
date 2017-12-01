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
      lsp-java
      log4j-mode
      (maven-pom-mode :location local)
      mvn
      ))

;; List of packages to exclude.
(setq appleshan-java-excluded-packages '())

;; Java support for lsp-mode using the Eclipse JDT Language Server.
;; Install:
;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; tar jdt-language-server-latest.tar.gz -C ~/.spacemacs.d/eclipse.jdt.ls/server/
(defun appleshan-java/init-lsp-java ()
  (use-package lsp-java
    :commands lsp-java-enable
    :init
    (setq lsp-java-server-install-dir "~/.spacemacs.d/eclipse.jdt.ls/server/")
    (add-hook 'java-mode-hook #'lsp-java-enable)))

(defun appleshan-java/init-log4j-mode ()
  (use-package log4j-mode
    :init
    (autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
      (add-hook 'log4j-mode-hook
                (lambda ()
                  (setq truncate-lines t)
                  (text-scale-set -1)
                  (toggle-read-only t)
                  (buffer-disable-undo)
                  (end-of-buffer)))
      )))

(defun appleshan-java/init-maven-pom-mode ()
  (use-package maven-pom-mode
    :defer t))

(defun appleshan-java/init-mvn ()
  (use-package mvn
    :defer t))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
