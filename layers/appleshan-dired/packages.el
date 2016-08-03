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
      diff-hl
      (dired :location built-in)
      dired+
      dired-k
      dired-sort
      ; dired-toggle-sudo
      ))

;; List of packages to exclude.
(setq appleshan-dired-excluded-packages '())

(defun appleshan-dired/post-init-diff-hl ()
  (with-eval-after-load 'diff-hl
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(defun appleshan-dired/post-init-dired ()
  (use-package dired
    :defer t
    :config
    (progn
      (when (fboundp 'global-dired-hide-details-mode)
        (global-dired-hide-details-mode -1))

      ;; allow dired to be able to delete or copy a whole dir.
      (setq dired-recursive-copies  'always) ; "always" means no asking
      (setq dired-recursive-deletes 'top)    ; "top" means ask once for top level directory

      ;; l: Is the only mandatory one.
      ;; a: Means to list invisible files.
      ;; G: Don't show group information.
      ;;    These days, when there are more laptops than people,
      ;;    the group info is rarely useful.
      ;; h: Human readable sizes, such as M for mebibytes.
      ;; 1v: Affects the sorting of digits, hopefully in a positive way.
      ;; --group-directories-first: self-explanatory, I like to have
      ;;    the directories on the top, separate from the files.
      ;; 另外注意：在 dired-sort 中会拼接参数，
      ;; 所以 "--group-directories-first" 必须写在 "-laGh1v" 前面.
      (setq dired-listing-switches "--group-directories-first -laGh1v")

      (setq directory-free-space-args "-Phk")
      (setq dired-auto-revert-buffer t)

      (setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

      (add-hook 'dired-after-readin-hook 'appleshan-dired/list-dir-first)

      (add-hook 'dired-mode-hook 'appleshan-dired/dired-hook)

      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        "f"         (if (configuration-layer/layer-usedp 'ivy)
                      'counsel-find-file
                    'helm-find-files)
        "="         'appleshan-dired/dired-diff
        "~"   '(lambda ()(interactive) (find-alternate-file "~/"))
        ;(kbd "M-o") 'dired-omit-mode
        "I" 'dired-omit-mode
        "DEL" 'appleshan-dired/up-directory ; DEL 意指退格键 (不是 删除(Delete) key)
        "-" 'appleshan-dired/up-directory
        "z" 'appleshan-dired/get-size)
    )))

;; TODO: slow!!!!
(defun appleshan-dired/init-dired+ ()
  (use-package dired+
    :defer t
    :init
    (progn
      ;; 顯示/隱藏檔案細節
      (setq diredp-hide-details-initially-flag nil)
      (setq diredp-hide-details-propagate-flag t)
      ;; 在补全 file 时忽略大小写的差别
      (setq read-file-name-completion-ignore-case t)
      ;; use single buffer for all dired navigation
      (toggle-diredp-find-file-reuse-dir 1)
      ;; disable font themeing from dired+
      ; (setq font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
      )))

(defun appleshan-dired/init-dired-k ()
  "Git status in dired."
  (use-package dired-k
    :defer t
    :init (require 'dired-k)
    :config
    (progn
      ;; always execute dired-k when dired buffer is opened
      (add-hook 'dired-initial-position-hook 'dired-k)

      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        "g" 'dired-k
        "K" 'dired-k)
      )))

(defun appleshan-dired/init-dired-sort ()
  (use-package dired-sort
    :defer t
    :init (require 'dired-sort)
    :config
    (add-hook 'dired-mode-hook 'appleshan-dired/dired-sort-hook)))

;; TODO: very slow!!!!
; (defun appleshan-dired/init-dired-toggle-sudo ()
;   (use-package dired-toggle-sudo
;     :defer t
;     :init (require 'dired-toggle-sudo)
;     :config
;     (progn
;       (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
;       (eval-after-load 'tramp
;         '(progn
;            ;; Allow to use: /sudo:user@host:/path/to/file
;            (add-to-list 'tramp-default-proxies-alist
;                         '(".*" "\\`.+\\'" "/ssh:%h:"))))
;       )))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
