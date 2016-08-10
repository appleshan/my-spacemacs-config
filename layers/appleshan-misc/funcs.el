;;; funcs.el --- appleshan-misc Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; 关闭水平滚动条
; (toggle-horizontal-scroll-bar)

;; {{ locale
(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p
        (and (executable-find "locale")
             (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; }}

(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq url-show-status nil)

;; cleanup recent files
(add-hook 'kill-emacs-hook
  #'(lambda () (progn
                 (and (fboundp 'recentf-cleanup)
                      (recentf-cleanup))
                 (and (fboundp 'projectile-cleanup-known-projects)
                      (projectile-cleanup-known-projects)))))

;; Transparency by default
(set-frame-parameter (selected-frame) 'alpha
                     (cons dotspacemacs-active-transparency
                           dotspacemacs-inactive-transparency))

;; Display visited file's path in the frame title
;; @See http://emacsredux.com/blog/2013/04/07/display-visited-files-path-in-the-frame-title/
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                   "%b"))))

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

(defun appleshan-misc/emacs-debug-init ()
  (interactive)
  (call-process "/opt/emacs-24.5/bin/emacs-24.5" nil 0 nil "--debug-init")
  (message "Started 'emacs --debug-init' - it will be ready soon ..."))

;; {{ 文件相关设置
;; 不自动添加换行符到末尾, 有些情况会出现错误，如果需要手动添加
(setq require-final-newline nil)

;; 在补全 buffer 时忽略大小写的差别
(setq read-buffer-completion-ignore-case t)

;; 只有当打开的文件超过100MB时，才产生警告
(setq large-file-warning-threshold 100000000)

(defun appleshan-misc/check-large-file ()
  "improve the performance of opening large file."
  (when (> (buffer-size) 500000)
    (progn
      (fundamental-mode)
      (hl-line-mode -1))))

(add-hook 'find-file-hook 'appleshan-misc/check-large-file)

(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;; remove all the duplicated emplies in current buffer
(defun appleshan-misc/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))
;; }}

;; {{ scroll functions
(defun appleshan-misc/hold-line-scroll-up()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-up 1)
    (line-move-to-column tmp)
    (forward-line 1)))

(defun appleshan-misc/hold-line-scroll-down()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-down 1)
    (line-move-to-column tmp)
    (forward-line -1)))
;; }}

;; {{ Move Current Line Up or Down
;; @see http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun appleshan-misc/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun appleshan-misc/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
;; }}

;; {{ insert date and time
(defun appleshan-misc/now ()
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun appleshan-misc/today ()
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))
;; }}

(defun open-readme-in-git-root-directory ()
  (interactive)
  (let (filename
        (root-dir
          (locate-dominating-file
            (file-name-as-directory
              (file-name-directory buffer-file-name)) ".git"))
        )
    ;; (message "root-dir=%s" root-dir)
    (and root-dir (file-name-as-directory root-dir))
    (setq filename (concat root-dir "README.org"))
    (if (not (file-exists-p filename))
        (setq filename (concat root-dir "README.md"))
      )
    ;; (message "filename=%s" filename)
    (if (file-exists-p filename)
        (switch-to-buffer (find-file-noselect filename nil nil))
      (message "NO README.org or README.md found!"))
    ))
(global-set-key (kbd "C-c C-f") 'open-readme-in-git-root-directory)

(defun appleshan/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "appleshan-layout")))

(defun appleshan/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "appleshan-layout")))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
