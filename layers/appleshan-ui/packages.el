;;; packages.el --- appleshan-ui Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq appleshan-ui-packages
    '(
      beacon
      visual-regexp
      visual-regexp-steroids
      ))

;; List of packages to exclude.
(setq appleshan-ui-excluded-packages '())

;; 不会丢失你的光标
(defun appleshan-ui/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")
      (spacemacs/toggle-beacon-on))
    :config
    (spacemacs|hide-lighter beacon-mode)))

;; https://github.com/benma/visual-regexp.el
;; https://github.com/benma/visual-regexp-steroids.el

(defun appleshan-ui/init-visual-regexp ()
  (use-package visual-regexp
    :defer t))

(defun appleshan-ui/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :bind (("C-M-%" . vr/replace)
           ("M-%"   . vr/query-replace)
           ("C-M-r"   . vr/isearch-backward)
           ("C-M-s"   . vr/isearch-forward)
           ("C-s" . isearch-forward)  ; ordinary forward search
           ("C-r" . isearch-backward) ; ordinary backward search
           ("C-c m" . vr/mc-mark)  ; for multiple-cursors
           )))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
