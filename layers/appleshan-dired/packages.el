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
      dired-k
      dired-sort
      peep-dired
      ))

;; List of packages to exclude.
(setq appleshan-dired-excluded-packages '())

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

      ; 传给 ls 的参数:
      ; a: Means to list invisible files.
      ; G: Don't show group information.
      ;    These days, when there are more laptops than people,
      ;    the group info is rarely useful.
      ; h: Human readable sizes, such as M for mebibytes.
      ; l: Is the only mandatory one.
      ; 1v: Affects the sorting of digits, hopefully in a positive way.
      ; u: sort by access time, newest first
      ; --group-directories-first: self-explanatory, I like to have
      ;    the directories on the top, separate from the files.
      ;
      ; 另外注意：在 dired-sort 中会拼接参数，
      ; 所以 "--group-directories-first" 必须写在 "-laGh1v" 前面.
      (setq dired-listing-switches "--group-directories-first -aGhl1vu")

      (setq directory-free-space-args "-Phk") ;目录空间选项
      (setq dired-auto-revert-buffer t)

      (setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

      (add-hook 'dired-mode-hook 'appleshan//dired-hook)

      ; hack for spacemacs
      (if (configuration-layer/layer-usedp 'ivy)
          (progn
            (evil-define-key 'normal dired-mode-map "J" 'counsel-find-file)
            (define-key dired-mode-map "j" 'counsel-find-file))
        (progn
          (evil-define-key 'normal dired-mode-map "J" 'spacemacs/helm-find-files)
          (define-key dired-mode-map "j" 'spacemacs/helm-find-files)))

      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        "I"   'dired-omit-mode
        "DEL" 'vinegar/up-directory
        )
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
      (add-hook 'dired-after-readin-hook #'dired-k-no-revert)

      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        "K" 'dired-k)
      )))

(defun appleshan-dired/init-dired-sort ()
  (use-package dired-sort
    :defer t
    :init (require 'dired-sort)
    :config
    (add-hook 'dired-mode-hook 'appleshan//dired-sort-hook)))

(defun appleshan-dired/init-peep-dired ()
  "preview files in dired"
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file
               peep-dired-prev-file)
    :bind (:map dired-mode-map
                ("P" . peep-dired))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
