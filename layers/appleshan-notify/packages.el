;;; packages.el --- appleshan-notify layer packages file for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是 appleshan 个人专用的 emacs 配置文件，emacs 中文用户可以参考。

;;; License: GPLv3

;;; Code:

(setq appleshan-notify-packages
      '(
        alert
        sauron
        ))

;; List of packages to exclude.
(setq appleshan-notify-excluded-packages '())

(defun appleshan-notify/init-alert ()
  (use-package alert
    :ensure t
    :config
    (when (eq system-type 'darwin)
      (setq alert-default-style 'notifier))
    (when (eq system-type 'gnu/linux)
      (setq alert-default-style 'notifications))))

;; The all-seeing eye of Sauron
(defun appleshan-notify/init-sauron ()
  (use-package sauron
    :ensure t
    :init
    (when (eq system-type 'darwin)
      ;; Remove dbus if on OSX
      (require 'sauron)
      (setq sauron-modules (remove 'sauron-dbus sauron-modules)))

    (setq sauron-max-line-length 120
          sauron-watch-patterns '("dakrone" "thnetos" "okenezak")
          sauron-watch-nicks '("dakrone" "thnetos")
          sauron-nick-insensitivity 20
          sauron-frame-geometry "120x36+0+0")
    ;; filter out IRC spam
    (defun tsp/hide-irc-user-spam (origin priority msg &optional properties)
      (or (string-match "^*** Users" msg)))
    (defun tsp/hide-tweet-counts (origin priority msg &optional properties)
      (or (string-match "^[0-9]+ new tweets" msg)))
    (add-hook 'sauron-event-block-functions #'tsp/hide-irc-user-spam)
    (add-hook 'sauron-event-block-functions #'tsp/hide-tweet-counts)

    (sauron-start-hidden)
    ;; Need to stop tracking notifications, because sauron will be sending
    ;; notifications!
    (sauron-notifications-stop)
    (add-hook 'sauron-event-added-functions 'sauron-alert-el-adapter)
    :commands (sauron-toggle-hide-show)
    :bind ("M-o" . sauron-toggle-hide-show)
    :config
    ;; Add the unread sauron notification count to the modeline
    (add-to-list 'global-mode-string '(cdr (sauron-count-events)))

    (defun eos/compilation-finish (buffer msg)
      "Send a sauron notification for compilation completing"
      (interactive)
      (sauron-add-event 'compilation
                        3
                        (format "[%s]: %s" buffer msg)
                        (lambda () (switch-to-buffer-other-window "*compilation*"))
                        nil))
    (add-to-list 'compilation-finish-functions #'eos/compilation-finish)

    (defun finish ()
      "Generic function for signaling something is \"done\"."
      (interactive)
      (sauron-add-event major-mode
                        3
                        (concat "Finished command in " (buffer-name))
                        (lambda () (switch-to-buffer-other-window (buffer-name)))
                        nil))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
