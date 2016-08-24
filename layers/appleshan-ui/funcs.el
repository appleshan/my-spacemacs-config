;;; funcs.el --- appleshan-ui Layer functions File for Spacemacs

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

;; Transparency by default
(set-frame-parameter (selected-frame) 'alpha
                     (cons dotspacemacs-active-transparency
                           dotspacemacs-inactive-transparency))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
