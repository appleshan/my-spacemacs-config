;;; keybindings.el --- appleshan-javascript Layer configuration File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

; xref-js2 uses the xref, so the same keybindings and UI as other xref backends is used:
; M-. Jump to definition
; M-? Jump to references
; M-, Pop back to where M-. was last invoked

(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "C-c m") #'appleshan-javascript/mdn-search))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; keybindings.el ends here
