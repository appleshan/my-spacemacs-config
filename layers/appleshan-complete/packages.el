;;; packages.el --- appleshan-complete layer packages file for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是 appleshan 个人专用的 emacs 配置文件，emacs 中文用户可以参考。

;;; License: GPLv3

;;; Code:

(setq appleshan-complete-packages
    '(
      company
      swiper
      yasnippet
      ))

;; List of packages to exclude.
(setq appleshan-complete-excluded-packages '())

;; company-mode
(defun appleshan-complete/post-init-company ()
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.5)

  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)

  ;; company-dabbrev
  (setq company-dabbrev-char-regexp "[[:word:]_:@.-]+")
  (setq company-dabbrev-minimum-length 2)

  (when (configuration-layer/package-usedp 'company)
    (spacemacs|add-company-hook shell-script-mode)
    (spacemacs|add-company-hook sh-mode)
    (spacemacs|add-company-hook nxml-mode)
    (spacemacs|add-company-hook conf-unix-mode)
    (spacemacs|add-company-hook json-mode))
  )

(defun appleshan-complete/post-init-swiper ()
  (setq ivy-display-style 'fancy)

  (evilified-state-evilify ivy-occur-mode ivy-occur-mode-map))

(defun appleshan-complete/post-init-yasnippet ()
  (set-face-background 'secondary-selection "gray")
  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
