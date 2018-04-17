;;; funcs.el --- appleshan-core Layer functions File for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License: GPLv3

;;; Code:

;; tramp, for sudo access
;; very slow!!!!
;; for profiling emacs --debug-init --timed-requires --profile
;; (require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
;; (setq tramp-default-method "ssh")         ; 设置传送文件默认的方法
;; (custom-set-variables '(tramp-verbose 0)) ; 设置tramp的响应方式, 关闭后不弹出消息

;; This line has very bad performance lose!!!!!!!!!!!!!!!!!!!
;; (set-default 'imenu-auto-rescan t)
(set-default 'imenu-auto-rescan nil)

;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; {{ Charset 设置
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

; (when (display-graphic-p)
;   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; @see https://github.com/hick/emacs-chinese
(defun appleshan/set-language-coding-systems ()
  ;; Always, always, prefer UTF-8, anything else is insanity
  (when (or (spacemacs/system-is-linux) (locale-is-utf8-p))
    ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
    (setq utf-translate-cjk-mode nil)

    ;; 影响 chinese-pyim, 造成不能输入中文的故障
    ;; 设置为中文简体语言环境
    (set-language-environment "UTF-8")
    (setq locale-coding-system 'utf-8)

    (set-charset-priority 'unicode)

    ;; 设置键盘输入时的字符编码
    (set-keyboard-coding-system 'utf-8-unix)
    (set-selection-coding-system 'utf-8-unix)
    (set-next-selection-coding-system 'utf-8-unix)

    ;; 文件默认保存为 utf-8
    (set-default-coding-systems 'utf-8-unix)
    (set-buffer-file-coding-system 'utf-8-unix)
    (set-default buffer-file-coding-system 'utf8-unix)

    ;; 解决粘贴中文出现乱码的问题
    (set-clipboard-coding-system 'utf-8-unix)

    ;; 终端中文乱码
    (set-terminal-coding-system 'utf-8-unix)
    (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
    ;(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
    (modify-coding-system-alist 'process "*" 'utf-8-unix)

    ;; 解决文件目录的中文名乱码
    (setq-default pathname-coding-system 'utf-8-unix)
    (set-file-name-coding-system 'utf-8-unix)

    (define-coding-system-alias 'UTF-8 'utf-8)
    )

  (when (spacemacs/system-is-mswindows)
    ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
    (setq utf-translate-cjk-mode nil)

    ;; 设置为中文简体语言环境
    (set-language-environment "UTF-8")
    (setq locale-coding-system 'utf-8)

    (set-charset-priority 'unicode)

    ;; 设置键盘输入时的字符编码
    (set-keyboard-coding-system 'utf-8-unix)
    (set-selection-coding-system 'utf-8-unix)
    (set-next-selection-coding-system 'utf-8-unix)

    ;; 文件默认保存为 utf-8
    (set-buffer-file-coding-system 'utf-8)
    (set-default buffer-file-coding-system 'utf8)
    (set-default-coding-systems 'utf-8)

    ;; 解决粘贴中文出现乱码的问题
    (set-clipboard-coding-system 'utf-8-unix)

    ;; 终端中文乱码
    (set-terminal-coding-system 'utf-8)
    (setq default-process-coding-system '(utf-8 . utf-8))
    ;(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
    (modify-coding-system-alist 'process "*" 'utf-8)

    ;; 解决文件目录的中文名乱码
    (setq-default pathname-coding-system 'utf-8)
    (set-file-name-coding-system 'utf-8)
    )

  ;; @see https://liu233w.github.io/blog/2016/09/29/org-python-windows/
  ; (when (spacemacs/system-is-mswindows)
  ;   (set-language-environment "chinese-gbk")
  ;   (set-terminal-coding-system 'gbk)
  ;   ;;
  ;   (modify-coding-system-alist 'process "*" 'gbk)
  ;   (defun liu233w/windows-shell-mode-coding ()
  ;     (set-buffer-file-coding-system 'gbk)
  ;     (set-buffer-process-coding-system 'gbk 'gbk))
  ;   (add-hook 'shell-mode-hook #'liu233w/windows-shell-mode-coding)
  ;   (add-hook 'inferior-python-mode-hook #'liu233w/windows-shell-mode-coding)
  ;   ;;
  ;   (defun liu233w//python-encode-in-org-babel-execute (func body params)
  ;     "org-babel 执行代码时不会自动编码文件，这里通过动态作用域覆盖默认选项来编码文件。"
  ;     ;; 此问题的详细信息请参考：https://github.com/Liu233w/.spacemacs.d/issues/6
  ;     (let ((coding-system-for-write 'utf-8))
  ;       (funcall func body params)))
  ;   (advice-add #'org-babel-execute:python :around
  ;               #'liu233w//python-encode-in-org-babel-execute)
  ;   )
  )
;; }}

;;{{
(defun copy-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
        (cond
         ((and (display-graphic-p) x-select-enable-clipboard)
          (x-set-selection 'CLIPBOARD (buffer-substring (region-beginning) (region-end))))
         (t (shell-command-on-region (region-beginning) (region-end) "xsel -ib")))
        (message "Yanked region to clipboard!")
        (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))

(defun paste-from-x-clipboard()
  (interactive)
  (cond
   ((and (display-graphic-p) x-select-enable-clipboard)
    (insert (x-get-selection 'CLIPBOARD)))
   (t (shell-command "xsel -ob" 1))
   ))

(defun appleshan/paste-in-minibuffer ()
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard))

(add-hook 'minibuffer-setup-hook 'appleshan/paste-in-minibuffer)
;;}}

;;http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun appleshan/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; Don't delete *scratch* buffer
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;;Don’t ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;Don’t ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; 在特定地模式下粘贴时自动缩进
(defadvice yank (after indent-region activate)
  "To make yank content indent automatically."
  (if (member major-mode '(emacs-lisp-mode
                           scheme-mode
                           lisp-mode
                           lisp-interaction-mode
                           sh-mode))
      (indent-region (region-beginning) (region-end) nil)))

;; {{ 文件相关设置
(defun appleshan/check-large-file ()
  "improve the performance of opening large file."
  (when (> (buffer-size) 500000)
    (progn
      (fundamental-mode)
      (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      (linum-mode -1)))

(add-hook 'find-file-hook 'appleshan/check-large-file)

(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

;; cleanup recent files
(defun appleshan/cleanup-recentf-and-known-projects ()
  (progn
    (and (fboundp 'recentf-cleanup)
         (recentf-cleanup))
    (and (fboundp 'projectile-cleanup-known-projects)
         (projectile-cleanup-known-projects))))

(add-hook 'kill-emacs-hook #'appleshan/cleanup-recentf-and-known-projects)

;; remove all the duplicated emplies in current buffer
(defun appleshan/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;; remove trailing whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; }}

(defun appleshan/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defvar bad-cleanup-modes '(python-mode yaml-mode)
  "List of modes where `cleanup-buffer' should not be used")

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a
buffer. If the buffer is one of the `bad-cleanup-modes' then no
re-indenting and un-tabification is done."
  (interactive)
  (unless (member major-mode bad-cleanup-modes)
    (progn
      (indent-buffer)
      (untabify-buffer)))
  (delete-trailing-whitespace))

;;{{ @see http://oremacs.com/2014/12/23/upcase-word-you-silly/
(defadvice upcase-word (before upcase-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))

(defadvice downcase-word (before downcase-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))

(defadvice capitalize-word (before capitalize-word-advice activate)
  (unless (looking-back "\\b")
    (backward-word)))
;;}}

;;{{ @see http://oremacs.com/2014/12/25/ode-to-toggle/
;; capitalize-word-toggle
(defun char-upcasep (letter)
  (eq letter (upcase letter)))

(defun capitalize-word-toggle ()
  (interactive)
  (let ((start
         (car
          (save-excursion
            (backward-word)
            (bounds-of-thing-at-point 'symbol)))))
    (if start
        (save-excursion
          (goto-char start)
          (funcall
           (if (char-upcasep (char-after))
               'downcase-region
             'upcase-region)
           start (1+ start)))
      (capitalize-word -1))))
(global-set-key (kbd "C->") 'capitalize-word-toggle)

;; upcase-word-toggle
(defun upcase-word-toggle ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        beg end
        (regionp
         (if (eq this-command last-command)
             (get this-command 'regionp)
           (put this-command 'regionp nil))))
    (cond
      ((or (region-active-p) regionp)
       (setq beg (region-beginning)
             end (region-end))
       (put this-command 'regionp t))
      (bounds
       (setq beg (car bounds)
             end (cdr bounds)))
      (t
       (setq beg (point)
             end (1+ beg))))
    (save-excursion
      (goto-char (1- beg))
      (and (re-search-forward "[A-Za-z]" end t)
           (funcall (if (char-upcasep (char-before))
                        'downcase-region
                      'upcase-region)
                    beg end)))))
(global-set-key (kbd "C-<") 'upcase-word-toggle)
;;}}

;; Save a file as utf-8
(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun appleshan/turn-on-hl-line ()
  (interactive)
  (when eos/hl-line-enabled
    (hl-line-mode 1)))

(defun appleshan/turn-off-hl-line ()
  (interactive)
  (hl-line-mode -1))

(defvar spacemacs--profiler nil)
;;;###autoload
(defun spacemacs/toggle-profiler ()
  (interactive)
  (if (not spacemacs--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq spacemacs--profiler (not spacemacs--profiler)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
