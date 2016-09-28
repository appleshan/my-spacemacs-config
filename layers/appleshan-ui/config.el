;;; config.el --- appleshan-ui Layer functions File for Spacemacs

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
  (insert ";; Happy hacking apple!
;; 用 Emacs, 需: 忘记鼠标, 无视菜单.
"))

;; Display visited file's path in the frame title
;; @See http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      '("" " Apple.Shan - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                   "%b"))))

;; Experimenting with transparency
; (let ((tp (cons dotspacemacs-active-transparency
;                 dotspacemacs-inactive-transparency)))
;   (push `(alpha . ,tp) default-frame-alist)
;   (set-frame-parameter (selected-frame) 'alpha tp))

(add-hook 'prog-mode-hook 'linum-mode)

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      profiler-report-mode
                                      ffip-diff-mode
                                      dictionary-mode
                                      erc-mode
                                      browse-kill-ring-mode
                                      etags-select-mode
                                      dired-mode
                                      help-mode
                                      text-mode
                                      fundamental-mode
                                      jabber-roster-mode
                                      jabber-chat-mode
                                      inferior-js-mode
                                      inferior-python-mode
                                      inferior-scheme-mode
                                      twittering-mode
                                      compilation-mode
                                      weibo-timeline-mode
                                      woman-mode
                                      Info-mode
                                      calc-mode
                                      calc-trail-mode
                                      comint-mode
                                      gnus-group-mode
                                      inf-ruby-mode
                                      gud-mode
                                      org-mode
                                      org-agenda-mode
                                      vc-git-log-edit-mode
                                      log-edit-mode
                                      term-mode
                                      spacemacs-buffer-mode
                                      w3m-mode
                                      speedbar-mode
                                      gnus-summary-mode
                                      gnus-article-mode
                                      calendar-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes)
           "Stop the load of linum-mode for some major modes."
           (unless (member major-mode linum-mode-inhibit-modes-list)
             ad-do-it))
(ad-activate 'linum-on)

;; Change cursor color depending on mode
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

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

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; config.el ends here
