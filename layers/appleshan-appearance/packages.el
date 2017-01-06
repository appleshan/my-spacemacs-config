;;; packages.el --- appleshan-appearance Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq appleshan-appearance-packages
    '(
      ;; beacon
      ; django-theme
      hl-anything
      popwin
      visual-regexp
      visual-regexp-steroids
      ;; tabbar
      ))

;; List of packages to exclude.
(setq appleshan-appearance-excluded-packages '())

;; 不会丢失你的光标
(defun appleshan-appearance/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "tb")
      (spacemacs/toggle-beacon-on))
    :config
    (spacemacs|hide-lighter beacon-mode)))

(defun appleshan-appearance/post-init-hl-anything ()
  (progn
    (hl-highlight-mode -1)
    (spacemacs|add-toggle toggle-hl-anything
      :status hl-highlight-mode
      :on (hl-highlight-mode)
      :off (hl-highlight-mode -1)
      :documentation "Toggle highlight anything mode."
      :evil-leader "ths")))

(defun appleshan-appearance/post-init-popwin ()
  (progn
    ;; man
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)

    ;; Elisp
    (push '("*ielm*" :stick t) popwin:special-display-config)

    ;; python
    (push '("*Python*"   :stick t) popwin:special-display-config)
    (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)

    ;; prodigy
    (push '("*prodigy*" :stick t) popwin:special-display-config)

    ;; org-mode
    (push '("*Org tags*" :stick t :height 30) popwin:special-display-config)

    ;; Completions
    (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

    (push "*appleshan/run-current-file output*" popwin:special-display-config)
    (delete "*Async Shell Command*" popwin:special-display-config)))

;; https://github.com/benma/visual-regexp.el
;; https://github.com/benma/visual-regexp-steroids.el

(defun appleshan-appearance/init-visual-regexp ()
  (use-package visual-regexp
    ; :commands (vr/replace vr/query-replace) ; See the bind of init-visual-regexp-steroids.
    :defer t))

(defun appleshan-appearance/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :defer t
    :bind (("C-M-%" . vr/replace)
           ("M-%"   . vr/query-replace)
           ("C-M-r"   . vr/isearch-backward)
           ("C-M-s"   . vr/isearch-forward)
           ("C-s" . isearch-forward)  ; ordinary forward search
           ("C-r" . isearch-backward) ; ordinary backward search
           ("C-c m" . vr/mc-mark)  ; for multiple-cursors
           )))

(defun appleshan-appearance/init-tabbar ()
  (use-package tabbar
    :init (tabbar-mode 1)
    :config
    (tabbar-theme-setup)
    ))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
