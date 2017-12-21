;;; funcs.el --- appleshan-programming Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;{{ 让 emacs 自动給 script 加上可执行权限
;; 保存时,检查文件的第一行是否包含#!,若包含则给文件添加执行权限
(defun appleshan/maybe-make-executable ()
  "Maybe make file executable unless it is a backup file."
  (unless (backup-file-name-p buffer-file-name)
    (executable-make-buffer-file-executable-if-script-p)))

(add-hook 'after-save-hook 'appleshan/maybe-make-executable)
;;}}

;; add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     scheme-mode
                     python-mode
                     js-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

(defun appleshan/vcs-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (or (locate-dominating-file directory ".git")
        (locate-dominating-file directory ".svn")
        (locate-dominating-file directory ".hg"))))

(defun appleshan/goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

(defun appleshan/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun appleshan/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; Notify that the compilation is finished
; (defun notify-compilation-result(buffer msg)
;   "Notify that the compilation is finished,
; close the *compilation* buffer if the compilation is successful,
; and set the focus back to Emacs frame"
;   (if (string-match "^finished" msg)
;     (progn
;       (delete-windows-on buffer)
;       (tooltip-show "\n Compilation Successful :-) \n "))
;     (tooltip-show "\n Compilation Failed :-( \n "))
;   (setq current-frame (car (car (cdr (current-frame-configuration)))))
;   (select-frame-set-input-focus current-frame)
;   )

; (add-to-list 'compilation-finish-functions 'notify-compilation-result)

;;{{ 更好的 compile 命令
;; see https://github.com/lujun9972/emacs-document/blob/master/emacs-common/%E6%9B%B4%E5%A5%BD%E7%9A%84compile%E5%91%BD%E4%BB%A4.org
;; This gives a regular `compile-command' prompt.
(define-key prog-mode-map [C-f9] #'compile)
;; This just compiles immediately.
(define-key prog-mode-map [f9] #'endless/compile-please)

;; I'm not scared of saving everything.
(setq compilation-ask-about-save nil)
;; Stop on the first error.
(setq compilation-scroll-output 'next-error)
;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)

(defcustom endless/compile-window-size 105
  "Width given to the non-compilation window."
  :type 'integer
  :group 'endless)

(defun endless/compile-please (comint)
  "Compile without confirmation.
With a prefix argument, use comint-mode."
  (interactive "P")
  ;; Do the command without a prompt.
  (save-window-excursion
    (compile (eval compile-command) (and comint t)))
  ;; Create a compile window of the desired width.
  (pop-to-buffer (get-buffer "*compilation*"))
  (enlarge-window
   (- (frame-width)
      endless/compile-window-size
      (window-width))
   'horizontal))
;;}}

;; @see https://github.com/apg/mvn-el
(ignore-errors
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (if (boundp 'compilation-filter-start)
            (ansi-color-apply-on-region compilation-filter-start (point))))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;;{{ Better Comments or tooling as a time sink
;; @See http://cestlaz.github.io/posts/better-comments/
(make-face 'font-lock-comment-important)
(set-face-foreground 'font-lock-comment-important "#00ff00")

(make-face 'font-lock-comment-todo)
(set-face-foreground 'font-lock-comment-todo "#ff0000")

(make-face 'font-lock-comment-strike)
(set-face-attribute 'font-lock-comment-strike
        nil :strike-through t)

(defun add-custom-keywords()
  "adds a few special keywords"
  (font-lock-add-keywords
   nil
   '(("cx \\(.+\\)" 1 'font-lock-comment-strike prepend)
     ("ct \\(.+\\)" 1 'font-lock-comment-todo prepend)
     ("ci \\(.+\\)" 1 'font-lock-comment-important prepend)
     )))

(add-hook 'prog-mode-hook #'add-custom-keywords)
;;}}

;;{{ my fix for tab indent
(defun appleshan/indent-region(numSpaces)
  (progn
    ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

    ; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion                          ; restore the position afterwards
      (goto-char regionStart)                ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd)                  ; go to the end of region
      (setq end (line-end-position))         ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil)           ; restore the selected region
      )
  ))

(defun appleshan/tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (appleshan/indent-region 4) ; region was selected, call indent-region
    (insert "    ")                   ; else insert four spaces as expected
    ))

(defun appleshan/untab-region (N)
  (interactive "p")
  (appleshan/indent-region -4))

(defun appleshan/hack-tab-key ()
  (interactive)
  (local-set-key (kbd "<tab>") 'appleshan/tab-region)
  (local-set-key (kbd "<S-tab>") 'appleshan/untab-region)
  )

(add-hook 'prog-mode-hook 'appleshan/hack-tab-key)
;;}}

;; In programming modes, make sure things like FIXME and TODO are highlighted so they stand out:
;; TODO: asg
;; HACK: adsf
;; REFACTOR: asd
;; FIXME: adf
;; OPTIMIZE: asdf
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|REFACTOR\\): " 1 font-lock-warning-face t)
         ("\\<\\(HACK\\): " 1 '(:foreground "purple") t)
         ("\\<\\(FIXME\\|OPTIMIZE\\): " 1 '(:foreground "red") t)
         )))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; Execute the current file.
(defun appleshan/run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be emacs lisp, php, perl, python, ruby, javascript, bash, ocaml, Visual Basic.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2015-08-21"
  (interactive)
  (let* (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("py" . "python")
            ("py3" . "python3")
            ; ("js" . "node") ; node.js
            ("sh" . "bash")
            ))
         (ξfname (buffer-file-name))
         (ξfSuffix (file-name-extension ξfname))
         (ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
         (ξcmd-str (concat ξprog-name " \""   ξfname "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

    (if (string-equal ξfSuffix "el") ; special case for emacs lisp
        (load ξfname)
      (if ξprog-name
          (progn
            (message "Running…")
            (async-shell-command ξcmd-str "*appleshan/run-current-file output*"))
        (message "No recognized program file suffix for this file.")))))

(defun appleshan/magit-browse ()
  "Browse to the project's github URL, if available"
  (interactive)
  (let ((url (with-temp-buffer
               (unless (zerop (call-process-shell-command
                               "git remote -v" nil t))
                 (error "Failed: 'git remote -v'"))
               (goto-char (point-min))
               (when (re-search-forward
                      "github\\.com[:/]\\(.+?\\)\\.git" nil t)
                 (format "https://github.com/%s" (match-string 1))))))
    (unless url
      (error "Can't find repository URL"))
    (browse-url url)))

(defun appleshan/github-browse-commit ()
  "Show the GitHub page for the current commit."
  (interactive)
  (use-package github-browse-file
    :commands (github-browse-file--relative-url))

  (let* ((commit git-messenger:last-commit-id)
         (url (concat "https://github.com/"
                      (github-browse-file--relative-url)
                      "/commit/"
                      commit)))
    (github-browse--save-and-view url)
    (git-messenger:popup-close)))

(defun appleshan/magit-branch-pull-request (number &optional branch checkout)
  "Create a new branch from a Github pull request and show its log.
Read \"NR[:BRANCH-NAME] from the user.  If BRANCH-NAME is not
provided use \"pr-NR\".  Set \"master\" as the upstream.
Assume all pull requests can be found on \"upstream\".  With a
prefix argument checkout branch instead of showing its log."
  (interactive
   (let ((input (magit-read-string "Branch pull request (NR[:BRANCH-NAME])")))
     (if (string-match "\\([1-9][0-9]*\\)\\(?::\\(.+\\)\\)?" input)
         (list (match-string 1 input)
               (match-string 2 input)
               current-prefix-arg)
       (user-error "Invalid input"))))
  (unless branch
    (setq branch (format "pr-%s" number)))
  (magit-call-git "fetch" "upstream" (format "pull/%s/head:%s" number branch))
  (if checkout
      (magit-run-git "checkout" branch)
    (apply #'magit-log (list branch) (magit-log-arguments))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
