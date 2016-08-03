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
      ; diminish
      ))

;; List of packages to exclude.
(setq appleshan-ui-excluded-packages '())

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
    :config (spacemacs|hide-lighter beacon-mode)))

; (defun appleshan-ui/post-init-diminish ()
;   (progn
;     (with-eval-after-load 'hungry-delete
;       (diminish 'hungry-delete-mode))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
