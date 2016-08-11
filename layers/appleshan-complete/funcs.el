;;; funcs.el --- appleshan-complete Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(with-eval-after-load 'company
  (defmacro appleshan/toggle-company-backends (backend)
    "Push or delete the backend to company-backends"
    (let ((funsymbol (intern (format "appleshan/company-toggle-%S" backend))))
      `(defun ,funsymbol ()
         (interactive)
         (if (eq (car company-backends) ',backend)
             (setq-local company-backends (delete ',backend company-backends))
           (push ',backend company-backends)))))

  (appleshan|toggle-company-backends company-tern)
  )

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
  ;; http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
  ;; ;FIXME: make it work with zsh
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

  (spacemacs/set-leader-keys "sh" 'counsel-yank-bash-history)

  (defun my-find-file-in-git-repo (repo)
    (if (file-directory-p repo)
        (let* ((default-directory repo)
               (files (split-string
                        (shell-command-to-string
                          (format "cd %s && git ls-files" repo)) "\n" t)))
          (ivy-read "files:" files
                    :action 'find-file
                    :caller 'my-find-file-in-git-repo))
      (message "%s is not a valid directory." repo)))

  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
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

  (defun counsel-find-file-recent-directory ()
    "Find file in recent git repository."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
             (append (mapcar 'file-name-directory recentf-list)
                     ;; fasd history
                     (if (executable-find "fasd")
                         (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "directories:" collection
                :action 'my-find-file-in-git-repo
                :caller 'counsel-find-file-recent-directory)))

  ; (spacemacs/declare-prefix "d" "dir/dash/zeal")
  (spacemacs/set-leader-keys "fad" 'counsel-goto-recent-directory)
  (spacemacs/set-leader-keys "faf" 'counsel-find-file-recent-directory)
  )

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
