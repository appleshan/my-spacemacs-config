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
  (defmacro appleshan|toggle-company-backends (backend)
    "Push or delete the backend to company-backends"
    (let ((funsymbol (intern (format "appleshan/company-toggle-%S" backend))))
      `(defun ,funsymbol ()
         (interactive)
         (if (eq (car company-backends) ',backend)
             (setq-local company-backends (delete ',backend company-backends))
           (push ',backend company-backends)))))

  ; (appleshan|toggle-company-backends company-tern)
  )

(with-eval-after-load 'swiper
  (defun my-swiper-search (p)
    (interactive "P")
    (let ((current-prefix-arg nil))
      (call-interactively
        (if p #'spacemacs/swiper-region-or-symbol
          #'swiper))))
  )

(with-eval-after-load 'ivy
  ;; http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
  ;; FIXME: make it work with zsh
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
)

(defvar helm-httpstatus-source
  '((name . "HTTP STATUS")
    (candidates . (("100 Continue")
                   ("101 Switching Protocols")
                   ("102 Processing")
                   ("200 OK")
                   ("201 Created")
                   ("202 Accepted")
                   ("203 Non-Authoritative Information")
                   ("204 No Content")
                   ("205 Reset Content")
                   ("206 Partial Content")
                   ("207 Multi-Status")
                   ("208 Already Reported")
                   ("300 Multiple Choices")
                   ("301 Moved Permanently")
                   ("302 Found")
                   ("303 See Other")
                   ("304 Not Modified")
                   ("305 Use Proxy")
                   ("307 Temporary Redirect")
                   ("400 Bad Request")
                   ("401 Unauthorized")
                   ("402 Payment Required")
                   ("403 Forbidden")
                   ("404 Not Found")
                   ("405 Method Not Allowed")
                   ("406 Not Acceptable")
                   ("407 Proxy Authentication Required")
                   ("408 Request Timeout")
                   ("409 Conflict")
                   ("410 Gone")
                   ("411 Length Required")
                   ("412 Precondition Failed")
                   ("413 Request Entity Too Large")
                   ("414 Request-URI Too Large")
                   ("415 Unsupported Media Type")
                   ("416 Request Range Not Satisfiable")
                   ("417 Expectation Failed")
                   ("418 I'm a teapot")
                   ("421 Misdirected Request")
                   ("422 Unprocessable Entity")
                   ("423 Locked")
                   ("424 Failed Dependency")
                   ("425 No code")
                   ("426 Upgrade Required")
                   ("428 Precondition Required")
                   ("429 Too Many Requests")
                   ("431 Request Header Fields Too Large")
                   ("449 Retry with")
                   ("500 Internal Server Error")
                   ("501 Not Implemented")
                   ("502 Bad Gateway")
                   ("503 Service Unavailable")
                   ("504 Gateway Timeout")
                   ("505 HTTP Version Not Supported")
                   ("506 Variant Also Negotiates")
                   ("507 Insufficient Storage")
                   ("509 Bandwidth Limit Exceeded")
                   ("510 Not Extended")
                   ("511 Network Authentication Required")))
      (action . message)))

(defun helm-httpstatus ()
  (interactive)
  (helm-other-buffer '(helm-httpstatus-source) "*helm httpstatus*"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
