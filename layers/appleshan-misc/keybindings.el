;;; keybindings.el --- Appleshan Layer configuration File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(global-set-key [(control down)] 'appleshan-misc/hold-line-scroll-up)
(global-set-key [(control up)] 'appleshan-misc/hold-line-scroll-down)

(global-set-key (kbd "M-<up>") 'appleshan-misc/move-line-up)
(global-set-key (kbd "M-<down>") 'appleshan-misc/move-line-down)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; keybindings.el ends here
