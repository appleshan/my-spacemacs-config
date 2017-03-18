;;; config.el --- appleshan-org Layer configuration File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables
(defvar org-gtd-dir nil)

(defvar org-agenda-files nil)
(defvar org-agenda-span nil)

(defvar org-todo-keywords nil)
(defvar org-todo-keyword-faces nil)

(defvar org-capture-templates nil)

(defvar org-columns-default-format nil)

(defvar org-global-properties nil)

(defvar org-tag-alist nil)

;; Make org-mode friendly for Chinese chars.
(setq org-emphasis-regexp-components
      '(
        " 	('\"{"
        "- 	.,:!?;'\")}\\["
        "：，。、  \t('\"{" ;pre
        "- ：，。、 \t.,:!?;'\")}\\" ;post
        1 ; newline
        ))

;; 让 ispell 跳过某段文本不做拼写检查
(defun appleshan/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'appleshan/org-ispell)

;; icon
(setq my-icon-dir (concat dotspacemacs-directory "local/icons/"))
(setq org-agenda-category-icon-alist
      '(("[Ee]macs" (concat spacemacs-banner-directory "img/spacemacs.png") nil nil :ascent center)
        ("Gnus" (concat my-icon-dir "gnus.png") nil nil :ascent center)
        ("Org" (concat my-icon-dir "org.png") nil nil :ascent center)
        ("Medical" (concat my-icon-dir "medical.png") nil nil :ascent center)
        ("Music" (concat my-icon-dir "music.png") nil nil :ascent center)
        ("Trip" (concat my-icon-dir "trip.png") nil nil :ascent center)
        ("Train" (concat my-icon-dir "train.png") nil nil :ascent center)
        ("Reading" (concat my-icon-dir "book.png") nil nil :ascent center)
        ("\\(Holidays\\|Vacation\\)" (concat my-icon-dir "holidays.png") nil nil :ascent center)
        (".*" '(space . (:width (16))))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; config.el ends here
