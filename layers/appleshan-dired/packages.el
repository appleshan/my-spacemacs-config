;;; packages.el --- appleshan-dired Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq appleshan-dired-packages
    '(
      (dired :location built-in)
      (dired-x :location built-in)
      (dired-k :location local)
      dired-quick-sort
      ))

;; List of packages to exclude.
(setq appleshan-dired-excluded-packages '())

;; Directory operations
(defun appleshan-dired/post-init-dired ()
  (with-eval-after-load 'dired
    ;; allow dired to be able to delete or copy a whole dir.
    (setq dired-recursive-copies  'always) ; "always" means no asking
    (setq dired-recursive-deletes 'top)    ; "top" means ask once for top level directory

    ;; Show directory first
    ;; @see http://oremacs.com/2015/01/13/dired-options/
    ;; 传给 ls 的参数:
    ;; l: Is the only mandatory one.
    ;; a: Means to list invisible files.
    ;; G: Don't show group information. These days, when there are more laptops
    ;;    than people, the group info is rarely useful.
    ;; h: Human readable sizes, such as M for mebibytes.
    ;; 1v: Affects the sorting of digits, hopefully in a positive way.
    ;; u: sort by access time, newest first
    ;; --group-directories-first: self-explanatory, I like to have the directories
    ;; on the top, separate from the files.
    ;;
    ;; 另外注意：在 dired-sort 中会拼接参数，
    ;; 所以 "--group-directories-first" 必须写在 "-laGh1v" 前面.
    (setq dired-listing-switches "--group-directories-first -alh1vu")

    (setq directory-free-space-args "-Phk") ;目录空间选项
    (setq dired-auto-revert-buffer t)

    (setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

    (define-key dired-mode-map (kbd "DEL") 'vinegar/up-directory)
    (define-key dired-mode-map (kbd "O") 'dired/open-in-external-app)
    (define-key dired-mode-map (kbd "z") 'dired/get-size)
  ))

;; @see http://oremacs.com/2015/01/04/dired-nohup/
(defun appleshan-dired/post-init-dired-x ()
  (with-eval-after-load 'dired-x
    (setq dired-guess-shell-alist-user
          '(("\\.pdf\\'" "open")
            ("\\.docx\\'" "open")
            ("\\.\\(?:djvu\\|eps\\)\\'" "open")
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
            ("\\.\\(?:xcf\\)\\'" "open")
            ("\\.csv\\'" "open")
            ("\\.tex\\'" "open")
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"
             "open")
            ("\\.\\(?:mp3\\|flac\\)\\'" "open")
            ("\\.html?\\'" "open")
            ("\\.md\\'" "open")))

    (add-hook 'dired-mode-hook 'appleshan//dired-hook)
    ))

;; Highlights dired buffer like k
(defun appleshan-dired/init-dired-k ()
  "Git status in dired."
  (use-package dired-k
    :init
      (setq dired-k-padding 1)
      (setq dired-k-human-readable t)
    :config
    (progn
      ;; always execute dired-k when dired buffer is opened
      (add-hook 'dired-initial-position-hook 'dired-k)
      (add-hook 'dired-after-readin-hook #'dired-k-no-revert)

      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        "K" 'dired-k)
      )))

;; Quick sort dired buffers via hydra
;; bind key: `S'
(defun appleshan-dired/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :if (or (executable-find "gls") (executable-find "ls"))
    :init (dired-quick-sort-setup)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
