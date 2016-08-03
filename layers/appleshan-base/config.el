;;; config.el --- appleshan-base layer configuration file for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

;; Variables

;; Full name and email
(setq user-full-name "Apple Shan"
      user-mail-address "apple.shan@gmail.com")

;; Startup screen in spacemacs/init()
; (setq inhibit-startup-screen t)
; (setq initial-buffer-choice nil)
; (setq initial-scratch-message ";; This is *scratch* buffer.\n\n")

;; 使用空格缩进
; (setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

(global-font-lock-mode t)               ;语法高亮
(transient-mark-mode 1)                 ;标记高亮
(show-paren-mode t)                     ;显示括号匹配
(auto-compression-mode 1)               ;打开压缩文件时自动解压缩
(mouse-avoidance-mode "banish")         ;只要一操作鼠标自动闪开

(setq default-major-mode 'text-mode)    ;设置默认地主模式为 TEXT 模式
(setq x-select-enable-clipboard t)      ;支持 emacs 和外部程序的粘贴
(setq x-stretch-cursor t)               ;光标在 TAB 字符上会显示为一个大方块
; (setq max-specpdl-size 10000)           ;最大容量
(setq kill-ring-max 1024)               ;用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
(setq undo-outer-limit 5000000)         ;撤销限制
(setq mark-ring-max 1024)               ;设置的 mark ring 容量
(setq global-mark-ring-max 1024)        ;设置最大的全局标记容量
(setq message-log-max t)                ;设置 message 记录全部消息, 而不用截去
(setq read-quoted-char-radix 16)        ;设置 引用字符 的基数
(setq void-text-area-pointer nil)       ;禁止显示鼠标指针
(setq show-paren-style 'parentheses)    ;括号匹配显示但不是烦人的跳到另一个括号。
(setq blink-matching-paren nil)         ;当插入右括号时不显示匹配的左括号
(setq max-lisp-eval-depth 40000)        ; lisp 最大执行深度
(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
; @see ~/.emacs.d/layers/+distribution/spacemacs-base/packages.el:L359
; (setq enable-recursive-minibuffers t)   ; minibuffer 递归调用命令
; @see ~/.emacs.d/layers/+distribution/spacemacs-base/config.el:L187-188
; (setq eval-expression-print-length nil) ;设置执行表达式的长度没有限制
; (setq eval-expression-print-level nil)  ;设置执行表达式的深度没有限制
(setq history-delete-duplicates t)      ;删除 minibuffer 的重复历史
; @see ~/.emacs.d/layers/+distribution/spacemacs-base/config.el:L194
; (put 'narrow-to-region 'disabled nil)   ;开启变窄区域
(setq print-escape-newlines t)          ;显示字符窗中的换行符为 \n

;; Only mark helm buffers as useless
(setq spacemacs-useless-buffers-regexp '("\\*helm\.\+\\*"))

;;自定theme的颜色
(set-face-attribute 'default nil :foreground "#C7EDCC")
(set-face-attribute 'cursor nil :background "#000000")
(set-face-attribute 'font-lock-comment-face nil :foreground "#888888" :background "#292b2e")

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; config.el ends here
