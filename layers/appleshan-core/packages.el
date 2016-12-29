;;; packages.el --- appleshan-core layer packages file for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是 appleshan 个人专用的 emacs 配置文件，emacs 中文用户可以参考。

;;; License: GPLv3

;;; Code:

(setq appleshan-core-packages
    '(
      (mule :location built-in)
      (desktop :location built-in)
      (recentf :location built-in)
      (profiler :location built-in)
      (whitespace :location built-in)
      (calendar :location built-in)
      (ibuffer :location built-in)
      ))

;; List of packages to exclude.
(setq appleshan-core-excluded-packages '())

;; Charset 设置
(defun appleshan-core/init-mule ()
  (use-package mule
    :config
    (progn
      ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
      (setq utf-translate-cjk-mode nil)

      ;; Always, always, prefer UTF-8, anything else is insanity
      (when (or window-system (locale-is-utf8-p))
        ;; 影响 chinese-pyim, 造成不能输入中文的故障
        ;; set environment coding system
        ;; (set-language-environment "UTF-8")

        (set-charset-priority 'unicode)
        (set-default-coding-systems 'utf-8)
        (set-buffer-file-coding-system 'utf-8-unix)
        (set-file-name-coding-system 'utf-8-unix)
        (set-selection-coding-system 'utf-8-unix)
        (set-next-selection-coding-system 'utf-8-unix)
        (set-clipboard-coding-system 'utf-8-unix)
        (set-keyboard-coding-system 'utf-8-unix)
        (set-terminal-coding-system 'utf-8-unix)
        (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
        (setq locale-coding-system 'utf-8)

        ;; @see ~/.emacs.d/core/core-spacemacs.el:72
        ;; (prefer-coding-system 'utf-8)
        )
    )))

(defun appleshan-core/pre-init-desktop ()
  (spacemacs|use-package-add-hook desktop
    :post-config
    (progn
      (setq desktop-auto-save-timeout 600)

      ;; save a bunch of variables to the desktop file
      ;; for lists specify the len of the maximal saved data also
      (setq desktop-globals-to-save
            (append '((comint-input-ring        . 50)
                      (compile-history          . 30)
                      desktop-missing-file-warning
                      (dired-regexp-history     . 20)
                      (extended-command-history . 30)
                      (face-name-history        . 20)
                      (file-name-history        . 100)
                      (grep-find-history        . 30)
                      (grep-history             . 30)
                      (magit-read-rev-history   . 50)
                      (minibuffer-history       . 50)
                      (org-clock-history        . 50)
                      (org-refile-history       . 50)
                      (org-tags-history         . 50)
                      (query-replace-history    . 60)
                      (read-expression-history  . 60)
                      (regexp-history           . 60)
                      (regexp-search-ring       . 20)
                      register-alist
                      (search-ring              . 20)
                      (shell-command-history    . 50)
                      tags-file-name
                      tags-table-list)))

      (setq desktop-buffers-not-to-save
              (concat "\\("
                      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                      "\\)$"))
      )))

(defun appleshan-core/post-init-recentf ()
  "Save recently visited files"
  (use-package recentf
    :config
    (progn
      (setq recentf-auto-cleanup 600)
      (setq recentf-max-saved-items 300)
      (dolist (item '("/\\.git/.*\\'" ; Git contents
                      "COMMIT_MSG"
                      "COMMIT_EDITMSG"
                      "github.*txt$"
                      ".*png$"
                      "/tmp/"
                      "/ssh:"
                      "\\var\\'"
                      ".*\\.gz\\'"
                      "TAGS"
                      ".*-autoloads\\.el\\'"
                      "/elpa/"
                      "/ssh:"
                      "/sudo:"
                      "/scp:"
                      (expand-file-name my-org-gtd-directory) ; org-gtd files
                      "/spacemacs-master.d/.cache/"
                      "/spacemacs-master.d/snippets/"
                      "/opt/emacs*/.*\\.el\\'"))
        (add-to-list 'recentf-exclude item))
    )))

(defun appleshan-core/init-profiler ()
  (use-package profiler
    :init
    (evilified-state-evilify profiler-report-mode profiler-report-mode-map)
    :config
    (progn
      ;; 定义列的宽度
      (setq profiler-report-cpu-line-format
        '((75 left)
          (24 right ((19 right) (5 right)))))
      (setq profiler-report-memory-line-format
        '((75 left)
          (19 right ((14 right profiler-format-number)
               (5 right)))))
      )))

(defun appleshan-core/post-init-whitespace ()
  (progn
    ;; ;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
    (setq whitespace-line-column fill-column) ;; limit line length
    ;;https://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
    (setq whitespace-display-mappings
          ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
          '(
            (space-mark 32 [183] [46])           ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
            (newline-mark 10 [182 10])           ; 10 LINE FEED
            (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
            ))
    (setq whitespace-style '(face tabs trailing tab-mark ))

    (with-eval-after-load 'whitespace
      (set-face-attribute 'whitespace-tab nil
                          :background "#Adff2f"
                          :foreground "#00a8a8"
                          :weight 'bold)
      (set-face-attribute 'whitespace-trailing nil
                          :background "#e4eeff"
                          :foreground "#183bc8"
                          :weight 'normal)
      )
  ))

(defun appleshan-core/init-calendar ()
  (use-package calendar
    :defer t
    :config
    (progn
      ;; Month
      (setq calendar-month-name-array
        ["一月" "二月" "三月" "四月" "五月" "六月"
         "七月" "八月" "九月" "十月" "十一月" "十二月"])
      ;; Week days
      (setq calendar-day-name-array
            ["星期一" "星期二" "星期三" "星期四" "星期五" "星期六" "星期日"])
      ;; First day of the week
      (setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday
      )))

(defun appleshan-core/post-init-ibuffer ()
  (setq ibuffer-show-empty-filter-groups nil))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
