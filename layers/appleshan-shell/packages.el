;;; packages.el --- appleshan-shell layer packages file for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是 appleshan 个人专用的 emacs 配置文件，emacs 中文用户可以参考。

;;; License: GPLv3

;;; Code:

(setq appleshan-shell-packages
      '(
        exec-path-from-shell
        ))

;; List of packages to exclude.
(setq appleshan-shell-excluded-packages '())

(defun appleshan-shell/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :ensure t
    :defer t
    :init
    (progn
      (dolist (var '(;; my path
                     "JAVA_HOME"
                     "ORACLE_HOME"
                     "LD_LIBRARY_PATH"
                     "NLS_LANG"
                     ;; system path
                     "PATH"
                     "SSH_AUTH_SOCK"
                     "SSH_AGENT_PID"
                     "GPG_AGENT_INFO"
                     "LANG"
                     "LC_CTYPE"))
        (add-to-list 'exec-path-from-shell-variables var))
      (when (memq window-system '(mac ns x))
        (exec-path-from-shell-initialize))
      )))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
