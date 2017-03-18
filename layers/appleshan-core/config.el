;;; config.el --- appleshan-core layer configuration file for Spacemacs
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

;; personal information about me: full name and email
(setq user-full-name "Apple Shan"
      user-mail-address "apple.shan@gmail.com")

(setq auto-coding-regexp-alist
      (delete (rassoc 'utf-16be-with-signature auto-coding-regexp-alist)
              (delete (rassoc 'utf-16le-with-signature auto-coding-regexp-alist)
                      (delete (rassoc 'utf-8-with-signature auto-coding-regexp-alist)
                              auto-coding-regexp-alist))))

(setq-default indent-tabs-mode nil) ; 使用空格缩进
;; set TAB and indention
(setq-default indent-tabs-mode nil
              tab-width 4) ; 所有buffer默认使用4格缩进

(delete-selection-mode t)            ; delete the selection with a key press
; (global-font-lock-mode t)            ; Turn on syntax highlighting for all buffers
; If you change buffer, or focus, disable the current buffer's mark:
(transient-mark-mode 1)

;; Don't indicate empty lines or the end of a buffer with visual marks
;; (the lines are cleaned up automatically anyway)
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries nil)

;; @See https://emacs-china.org/t/smartparens/2055/3
(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

;; show parenthesis match
(show-paren-mode t)
(setq show-paren-style 'expression)

(auto-compression-mode t)            ;Transparently Open Compressed Files
;(mouse-avoidance-mode "banish")      ;只要一操作鼠标自动闪开

(setq-default major-mode 'text-mode) ;设置默认的主模式为 TEXT 模式
(setq x-select-enable-clipboard t)   ;支持 emacs 和外部程序的粘贴
(setq x-stretch-cursor t)            ;光标在 TAB 字符上会显示为一个大方块
; (setq max-specpdl-size 10000)        ;最大容量
(setq kill-ring-max 1024)            ;用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
(setq undo-outer-limit 5000000)      ;撤销限制
(setq mark-ring-max 1024)            ;设置的 mark ring 容量
(setq global-mark-ring-max 1024)     ;设置最大的全局标记容量
(setq message-log-max t)             ;设置 *Messages* 记录全部消息, 而不用截去
(setq read-quoted-char-radix 16)     ;设置 引用字符 的基数
(setq void-text-area-pointer nil)    ;禁止显示鼠标指针
(setq show-paren-style 'parentheses) ;括号匹配显示但不是烦人的跳到另一个括号。
(setq blink-matching-paren nil)      ;当插入右括号时不显示匹配的左括号
(setq max-lisp-eval-depth 40000)     ; lisp 最大执行深度
(setq-default comment-style 'indent) ;设定自动缩进的注释风格
(setq history-delete-duplicates t)   ;删除 minibuffer 的重复历史
(setq print-escape-newlines t)       ;显示字符窗中的换行符为 \n

;; Only mark helm buffers as useless
(setq spacemacs-useless-buffers-regexp '("\\*helm\.\+\\*"))

;; Marking the *Messages* buffer as useful
(push "\\*Messages\\*" spacemacs-useful-buffers-regexp)

;; Prevent the visual selection overriding my system clipboard
(fset 'evil-visual-update-x-selection 'ignore)

;; Don't move back the cursor one position when exiting insert mode
(setq evil-move-cursor-back nil)

(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq url-show-status nil)

;; Allow font-lock-mode to do background parsing
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-defer-time nil
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5
      jit-lock-stealth-time 16
      jit-lock-stealth-verbose nil)
(setq-default font-lock-multiline t)

;; Wait a bit longer than the default (0.5 seconds) before assuming Emacs is idle
(setq idle-update-delay 2)

(setq line-number-display-limit-width 10000)

;; Make gnutls a bit safer, the default is an absurdly low 256
(setq gnutls-min-prime-bits 4096)

;; Set up the fill-column to 80 characters and set tab width to 2
(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; Fix some weird color escape sequences
(setq system-uses-terminfo nil)

;; Resolve symlinks:
(setq-default find-file-visit-truename t)

;; Require a newline at the end of files:
(setq require-final-newline t)
;; add no new lines when "arrow-down key" at the end of a buffer
(setq next-line-add-newlines nil)

;; Ignore case when performing completion
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Don't warn me about large files unless they're at least 25mb:
(setq large-file-warning-threshold (* 25 1024 1024))

;; Switch to unified diffs by default:
;; (setq diff-switches "-u")

;; Set the internal calculator not to go to scientific form quite so quickly:
(setq calc-display-sci-low -5)

;; Don't bother saving things to the kill-ring twice, remove duplicates
(setq kill-do-not-save-duplicates t)

;; Preserve the window location when opening things
(setq switch-to-buffer-preserve-window-point t)

;; Use a sane re-builder syntax so I don't have to have crazy escapes,
;; see: https://masteringemacs.org/article/re-builder-interactive-regexp-builder
(setq reb-re-syntax 'string)

;; Turn on auto-fill mode in text buffers:
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(use-package diminish
;  :init (diminish 'auto-fill-function ""))

(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (global-prettify-symbols-mode +1))

;; 搜索时任意字符都可以用空格代替
(setq search-whitespace-regexp ".*?")

;; Setup up Shell Environment
;; @see http://ergoemacs.org/emacs/emacs_env_var_paths.html
;; for Windows 10
(when (spacemacs/system-is-mswindows)
  (let ((mypaths
          '(
            "D:/bin"
            "D:/portable-soft/PortableGit/bin"
            "D:/portable-soft/PortableGit/mingw64/libexec/git-core"
            "c:/Program Files (x86)/GnuPG/bin/"
           )))
    (setq exec-path (append mypaths (list "." exec-directory)) )
    (setenv "PATH" (mapconcat 'identity mypaths ";") )
    (setenv "HOME" "D:/" )
    (setenv "JAR_PATH" "D:/bin/java-lib" )
    ))

;; for linux
(when (spacemacs/system-is-linux)
  (let ((mypaths
          '(
            "/opt/oracle/lib"
            "/opt/jdk/jdk1.8.0_102/bin"
            "/opt/jdk/apache-maven-3.3.9/bin"
            "/opt/eclipse-jee-neon/eclipse"
            "/home/appleshan/bin"
            "/home/appleshan/bin/cli"
            "/home/appleshan/.local/bin"
            "/home/appleshan/perl5/bin"
            "/home/appleshan/bin/sift"
            "/usr/local/sbin"
            "/usr/local/bin"
            "/usr/sbin"
            "/usr/bin"
            "/sbin"
            "/bin"
            "/usr/games"
            "/usr/local/games"
           )))
    (setq exec-path (append mypaths (list "." exec-directory)))

    (setenv "PATH" (mapconcat 'identity mypaths ":") )
    (setenv "HOME" "/home/appleshan")
    (setenv "JAR_PATH" "/opt/java-lib" )
    )

  (setenv "LD_LIBRARY_PATH" "/opt/oracle/lib")
  (setenv "NLS_LANG" "SIMPLIFIED CHINESE_CHINA.AL32UTF8")
  )

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; config.el ends here
