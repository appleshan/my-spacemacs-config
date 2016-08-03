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

;; tramp, for sudo access
;; very slow!!!!
;; for profiling emacs --debug-init --timed-requires --profile
;; (require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
;; (setq tramp-default-method "ssh")         ; 设置传送文件默认的方法
;; (custom-set-variables '(tramp-verbose 0)) ; 设置tramp的响应方式, 关闭后不弹出消息

;; This line has very bad performance lose!!!!!!!!!!!!!!!!!!!
;; (set-default 'imenu-auto-rescan t)

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

;;Don’t ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;Don’t ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

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

;; Marking the *Messages* buffer as useful
; (push "\\*Messages\\*" spacemacs-useful-buffers-regexp)

(defun appleshan-misc/emacs-debug-init ()
  (interactive)
  (call-process "/opt/emacs-24.5/bin/emacs-24.5" nil 0 nil "--debug-init")
  (message "Started 'emacs --debug-init' - it will be ready soon ..."))

;; {{ 文件相关设置
;; 不自动添加换行符到末尾, 有些情况会出现错误，如果需要手动添加
(setq require-final-newline nil)

;; 在补全 buffer 时忽略大小写的差别
(setq read-buffer-completion-ignore-case t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defun appleshan-misc/check-large-file ()
  "improve the performance of opening large file."
  (when (> (buffer-size) 500000)
    (progn
      (fundamental-mode)
      (hl-line-mode -1))))

(add-hook 'find-file-hook 'appleshan-misc/check-large-file)

;; when save a buffer, the directory is not exsits, 
;; it will ask you to create the directory
(defun appleshan-misc/before-save-hook ()
  (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
                   (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
          (make-directory dir t)))))
(add-hook 'before-save-hook 'appleshan-misc/before-save-hook)

;; remove all the duplicated emplies in current buffer
(defun appleshan-misc/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;;http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun appleshan-misc/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
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

(defun appleshan-misc/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun appleshan-misc/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))

(defun open-readme-in-git-root-directory ()
  (interactive)
  (let (filename
        (root-dir (locate-dominating-file (file-name-as-directory (file-name-directory buffer-file-name)) ".git"))
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

;; TODO: 须要改成 linux 上的 growl
;; http://blog.lojic.com/2009/08/06/send-growl-notifications-from-carbon-emacs-on-osx/
(defun appleshan/growl-notification (title message &optional sticky)
  "Send a Growl notification"
  (do-applescript
   (format "tell application \"GrowlHelperApp\" \n
              notify with name \"Emacs Notification\" title \"%s\" description \"%s\" application name \"Emacs\" sticky \"%s\"
              end tell
              "
           title
           message
           (if sticky "yes" "no"))))

(with-eval-after-load 'swiper
  (defun my-swiper-search (p)
    (interactive "P")
    (let ((current-prefix-arg nil))
      (call-interactively
        (if p #'spacemacs/swiper-region-or-symbol
          #'swiper))))

  (define-key global-map (kbd "C-s") 'my-swiper-search)
  )

(with-eval-after-load 'ivy
  (defun counsel-yank-bash-history ()
    "Yank the bash history"
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r") ; reload history
      (setq collection
            (nreverse
              (split-string
                (with-temp-buffer
                  (insert-file-contents (file-truename "~/.bash_history"))
                  (buffer-string))
                "\n"
                t)))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection))
                             (car collection)
                             (ivy-read (format "Bash history:") collection))))
        (kill-new val)
        (message "%s => kill-ring" val))))

  ; (spacemacs/declare-prefix "S" "shell")
  (spacemacs/set-leader-keys "Sh" 'counsel-yank-bash-history)

  (defun counsel-goto-recent-directory ()
    "Open recent directory with dired"
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    ;; fasd history
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "directories:" collection
                :action 'dired
                :caller 'counsel-goto-recent-directory)))

  ; (spacemacs/declare-prefix "d" "dir/dash/zeal")
  (spacemacs/set-leader-keys "fad" 'counsel-goto-recent-directory)
  )

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
