;;; packages.el --- appleshan-remote layer packages file for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是 appleshan 个人专用的 emacs 配置文件，emacs 中文用户可以参考。

;;; License: GPLv3

;;; Code:

(setq appleshan-remote-packages
      '(
        (tramp :location built-in)
        ))

;; List of packages to exclude.
(setq appleshan-remote-excluded-packages '())

(defun appleshan-remote/init-tramp ()
  (use-package tramp
    :defer 5
    :config
    ;; Turn of auto-save for tramp files
    (defun tramp-set-auto-save ()
      (auto-save-mode -1))
    (with-eval-after-load 'tramp-cache
      (setq tramp-persistency-file-name "~/.emacs.d/etc/tramp"))
    (setq tramp-default-method "ssh"
          tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
          tramp-adb-program "adb"
          ;; use the settings in ~/.ssh/config instead of Tramp's
          tramp-use-ssh-controlmaster-options nil
          backup-enable-predicate
          (lambda (name)
            (and (normal-backup-enable-predicate name)
                 (not (let ((method (file-remote-p name 'method)))
                        (when (stringp method)
                          (member method '("su" "sudo"))))))))

    (use-package tramp-sh
      :config
      (add-to-list 'tramp-remote-path "/usr/local/sbin")
      (add-to-list 'tramp-remote-path "/opt/java/current/bin")
      (add-to-list 'tramp-remote-path "~/bin"))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
