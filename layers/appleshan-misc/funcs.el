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

;; cleanup recent files
(add-hook 'kill-emacs-hook
  #'(lambda () (progn
                 (and (fboundp 'recentf-cleanup)
                      (recentf-cleanup))
                 (and (fboundp 'projectile-cleanup-known-projects)
                      (projectile-cleanup-known-projects)))))

;; {{ scroll functions
(defun appleshan/hold-line-scroll-up()
  "Scroll the page with the cursor in the same line"
  (interactive)
  ;; move the cursor also
  (let ((tmp (current-column)))
    (scroll-up 1)
    (line-move-to-column tmp)
    (forward-line 1)))

(defun appleshan/hold-line-scroll-down()
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
(defun appleshan/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun appleshan/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
;; }}

;; {{ insert date and time
(defun appleshan/now ()
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun appleshan/today ()
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))
;; }}

(defun appleshan/open-readme-in-git-root-directory ()
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

(defun appleshan/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "appleshan-layout")))

(defun appleshan/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "appleshan-layout")))

(add-hook 'minibuffer-inactive-mode-hook
          '(lambda() (set (make-local-variable 'semantic-mode) nil)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
