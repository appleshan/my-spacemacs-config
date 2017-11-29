;;; packages.el --- appleshan-misc Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq appleshan-misc-packages
    '(
      bbdb-vcard
      browse-kill-ring
      ;; discover-my-major
      dockerfile-mode
      flyspell-correct
      multiple-cursors
      persistent-scratch
      tiny
      vlf
      ))

;; List of packages to exclude.
(setq appleshan-misc-excluded-packages '())

(defun appleshan-misc/init-bbdb-vcard ()
  (use-package bbdb-vcard
    :defer t
    :init
    (progn
      ;; import Gmail contacts in vcard format into bbdb
      (setq bbdb-file my-gmail-bbdb-file)

      (add-hook 'message-mode-hook
          '(lambda ()
             (enable-flyspell-mode-conditionally)
             (bbdb-initialize 'message)
             (bbdb-initialize 'gnus)
             (local-set-key "<TAB>" 'bbdb-complete-name)))

      (add-hook 'bbdb-initialize-hook
          '(lambda ()
             ;; @see http://emacs-fu.blogspot.com.au/2009/08/managing-e-mail-addresses-with-bbdb.html
             (setq
               bbdb-file-coding-system 'utf-8
               bbdb-complete-mail-allow-cycling t       ;; cycle through matches
               ;; this only works partially

               ;; auto-create addresses from mail
               bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
               bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
               ;; NOTE: there can be only one entry per header (such as To, From)
               ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

               '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter\\|notifications")))

               ;; just remove some warning since bbdb package hook the mail-mode
               (setq compose-mail-user-agent-warnings nil)
             ))
      )))

;; Interactively insert items from kill-ring
(defun appleshan-misc/init-browse-kill-ring ()
  (use-package browse-kill-ring
    :defer t
    :bind (("C-c y" . appleshan/browse-kill-ring))
    :config
    (progn
      (setq browse-kill-ring-highlight-current-entry t)
      (setq browse-kill-ring-separator
            (concat "\n" (make-string 70 ?=) "\n"))

      (add-hook 'browse-kill-ring-hook
                'appleshan/browse-kill-ring-settings)

      (defun appleshan/browse-kill-ring-settings ()
        (interactive)
        (setq browse-kill-ring-show-preview nil)
        (define-key browse-kill-ring-mode-map (kbd "C-c C-k") 'browse-kill-ring-quit)
        (define-key browse-kill-ring-mode-map (kbd "C-k") 'browse-kill-ring-quit)
        (define-key browse-kill-ring-mode-map (kbd "k") 'browse-kill-ring-quit)
        (define-key browse-kill-ring-mode-map (kbd "C-/") 'browse-kill-ring-quit)
        (define-key browse-kill-ring-mode-map (kbd "C-n") 'browse-kill-ring-forward)
        (define-key browse-kill-ring-mode-map (kbd "C-p") 'browse-kill-ring-previous)
        (define-key browse-kill-ring-mode-map (kbd "C-c C-c") 'browse-kill-ring-insert-and-quit)
        (define-key browse-kill-ring-mode-map (kbd "y") 'browse-kill-ring-insert-and-quit))

      (defun appleshan/browse-kill-ring ()
        (interactive)
        (let ((clipboard-output
               (if (string= system-type "windows-nt")
                   (w32-get-clipboard-data)
                 (x-get-clipboard))))
          (when (and clipboard-output
                     (not (string= (car kill-ring) clipboard-output)))
            (kill-new clipboard-output))
          (if (car kill-ring)
              (browse-kill-ring)
            (message "kill ring is empty"))))
      )))

;; Discover key bindings and their meaning for the current Emacs major mode
(defun appleshan-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map)
      )))

(defun appleshan-misc/post-init-dockerfile-mode ()
  (with-eval-after-load 'dockerfile-mode
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))))

(defun appleshan-misc/post-init-flyspell-correct ()
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)))

(defun appleshan-misc/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (setq mc/list-file (concat dotspacemacs-directory ".cache/.mc-lists.el")))
    :bind (("C-s-l" . mc/edit-lines)
           ("C-s-f" . mc/mark-all-dwim)
           ("C-s-." . mc/mark-next-like-this)
           ("C-s-," . mc/mark-previous-like-this)
           ("s->" . mc/unmark-next-like-this)
           ("s-<" . mc/unmark-previous-like-this)
           ("C-c C-s-." . mc/mark-all-like-this))))

(defun appleshan-misc/init-persistent-scratch ()
  (use-package persistent-scratch
    :defer t
    :init
    (progn
      (setq persistent-scratch-autosave-interval 30) ; 30 second
      (setq persistent-scratch-save-file (expand-file-name ".persistent-scratch" spacemacs-cache-directory)))
    :config
    (persistent-scratch-setup-default)))

(defun appleshan-misc/init-tiny ()
  (use-package tiny
    :defer t
    :init
    (spacemacs/set-leader-keys "oe" 'tiny-expand)))

(defun appleshan-misc/init-vlf ()
  (use-package vlf
    :init (require 'vlf-setup)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
