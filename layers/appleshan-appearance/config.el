;;; config.el --- appleshan-appearance Layer functions File for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License: GPLv3

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 测试字体：
;; 1lIi     <-- 数字1，小写字母l，大小写字母i
;; 0Oo      <-- 数字0，大小写字母o
;; '\"`     <-- 单引号，双引号，反引号
;; 0O l1 Z2 S5 G6 B8 71 lI vy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Happy hacking apple!
;; 用 Emacs, 需忘记鼠标, 无视菜单.
(with-current-buffer (get-buffer-create "*scratch*")
  (emacs-lisp-mode)
  (insert ";; Happy hacking appleshan!\n;; 用 Emacs, 需: 忘记鼠标, 无视菜单.\n\n"))

;; Display visited file's path in the frame title
;; @See http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      `(,(user-login-name) "@" ,(system-name) " | "
         (:eval (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
                  "%b"))))

;; Experimenting with transparency
(let ((tp (cons dotspacemacs-active-transparency
                dotspacemacs-inactive-transparency)))
  (push `(alpha . ,tp) default-frame-alist)
  (set-frame-parameter (selected-frame) 'alpha tp))

(add-hook 'prog-mode-hook 'linum-mode)

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      profiler-report-mode
                                      dictionary-mode
                                      browse-kill-ring-mode
                                      dired-mode
                                      help-mode
                                      text-mode
                                      fundamental-mode
                                      inferior-js-mode
                                      inferior-python-mode
                                      inferior-scheme-mode
                                      compilation-mode
                                      woman-mode
                                      Info-mode
                                      calc-mode
                                      calc-trail-mode
                                      comint-mode
                                      gud-mode
                                      org-mode
                                      org-agenda-mode
                                      vc-git-log-edit-mode
                                      log-edit-mode
                                      spacemacs-buffer-mode
                                      calendar-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes)
           "Stop the load of linum-mode for some major modes."
           (unless (member major-mode linum-mode-inhibit-modes-list)
             ad-do-it))
(ad-activate 'linum-on)

;; UI设置
;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01110000
   #b00010000
   #b00010000
   #b00000000])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;; Turn off all kinds of modes, I don't need the menu bar, or the tool bar:
;; (when (functionp 'menu-bar-mode)
;;   (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))
;; (when (functionp 'tooltip-mode)
;;   (tooltip-mode -1))
;; (when (functionp 'tool-bar-mode)
;;   (tool-bar-mode -1))
;; (when (functionp 'blink-cursor-mode)
;;   (blink-cursor-mode -1))

;; Hide the mouse while typing:
(setq make-pointer-invisible t)

;; Don't use dialog boxes, just ask inside Emacs
(setq use-dialog-box nil)

;; The number is in pixels.
(setq-default line-spacing 0)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;;set unicode font for mode-line
; (set-fontset-font t '(#x2776 . #x2793) "Lucida Sans Unicode") ;;win numbers
; (set-fontset-font t '(#x24b6 . #x24fe) "Lucida Sans Unicode") ;;circled letters
; (set-fontset-font t '(#x2295 . #x22a1) "Lucida Sans Unicode") ;;additional characters

(spacemacs|diminish which-key-mode)
(spacemacs|diminish spacemacs-whitespace-cleanup-mode)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; config.el ends here
