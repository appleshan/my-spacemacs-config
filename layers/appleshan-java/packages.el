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
      ; eclim
      ; (eclim-java-run :location local) ; in eclim
      ; (flycheck-eclim :location local)
      ; (flycheck-java :location local)
      ; (flycheck-java-maven :location local)
      ; project-explorer
      ; (ajoke :location local)
      ; log4j-mode
      (maven-pom-mode :location local)
      mvn
      meghanada
      (flycheck-meghanada :location local)
      ))

;; List of packages to exclude.
(setq appleshan-java-excluded-packages '())

;; https://github.com/ervandew/eclim.git
;; eclim install:
;; java -Dvim.skip=true -Declipse.home=/opt/java-ide/eclipse-jee-neon/eclipse -jar eclim_2.6.0.jar install

(defun appleshan-java/post-init-eclim ()
  (setq ; eclim-auto-save t
        eclim-autoupdate-problems nil
        eclim-eclipse-dirs "/opt/java-ide/eclipse-jee-neon/eclipse"
        eclim-executable (or (executable-find "eclim") "/opt/java-ide/eclipse-jee-neon/eclipse/eclim")
        eclimd-executable (or (executable-find "eclimd") "/opt/java-ide/eclipse-jee-neon/eclipse/eclimd")
        eclimd-wait-for-process nil
        ;; Specify the workspace to use by default
        eclimd-default-workspace "~/developer/projects/java/")
  )

(defun appleshan-java/init-eclim-java-run ()
  (use-package eclim-java-run
    :defer t
    :init (require 'eclim-java-run)))

(defun appleshan-java/init-flycheck-eclim ()
  (use-package flycheck-eclim
    :defer t
    :init
    (progn
      (add-hook 'java-mode-hook
                (lambda ()
                  (use-package eclim)
                  (require 'flycheck-eclim)
                  (flycheck-eclim-setup)
                  ))
      )
    ))

(defun appleshan-java/init-flycheck-java ()
  (use-package flycheck-java
    :defer t
    :init
    (progn
      (add-hook 'java-mode-hook
                (lambda ()
                  (setq flycheck-java-ecj-jar-path "/home/apple/bin/develop/java/ecj-4.5.1.jar")
                  (require 'flycheck-java)
                  ))
      )
    ))

(defun appleshan-java/init-flycheck-java-maven ()
  (use-package flycheck-java-maven
    :defer t
    :init
    (progn
      (add-hook 'java-mode-hook (lambda () (require 'flycheck-java-maven) )))
    ))

(defun appleshan-java/init-project-explorer ()
  (use-package project-explorer
    :defer t
    :init (require 'project-explorer)
    ))

(defun appleshan-java/init-ajoke ()
  (use-package ajoke
    :load-path "/home/apple/repo/git/java/ajoke/etc/elisp/"
    :defer t
    ; :init (require 'ajoke)
    ))

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

(defun appleshan-java/init-meghanada ()
  (use-package meghanada
    :defer t
    :init
    (progn
      (setq meghanada-server-install-dir "/opt/java-lib/meghanada")
      ;; Don't auto-start
      (setq meghanada-auto-start nil)
      ;; meghanada-mode on
      (add-hook 'java-mode-hook 'meghanada-mode)
      )))

(defun appleshan-java/init-flycheck-meghanada ()
  (use-package flycheck-meghanada
    :defer t
    :config
    (progn
      (add-hook 'java-mode-hook (lambda () (meghanada-flycheck-enable) )))
    ))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
