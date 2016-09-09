;;; keybindings.el --- appleshan Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(define-key global-map (kbd "<f8>") 'appleshan/show-current-buffer-major-mode)

;; dir
(spacemacs/set-leader-keys "drd" 'counsel-goto-recent-directory)
(spacemacs/set-leader-keys "drf" 'counsel-find-file-recent-directory)
(spacemacs/set-leader-keys "dt" 'neotree-dir)

;; bash
(spacemacs/set-leader-keys "ash" 'counsel-yank-bash-history)

(global-set-key (kbd "M-/") 'company-complete)

(global-set-key (kbd "C-\\") 'appleshan/evil-toggle-input-method)

(global-set-key [(control down)] 'appleshan/hold-line-scroll-up)
(global-set-key [(control up)] 'appleshan/hold-line-scroll-down)

(global-set-key (kbd "M-<up>") 'appleshan/move-line-up)
(global-set-key (kbd "M-<down>") 'appleshan/move-line-down)

(global-set-key [(shift return)] 'appleshan/smart-open-line)

; xref-js2 uses the xref, so the same keybindings and UI as other xref backends is used:
; M-. Jump to definition
; M-? Jump to references
; M-, Pop back to where M-. was last invoked

(with-eval-after-load 'js2-mode
  (define-key js2-mode-map (kbd "C-c m") #'appleshan/mdn-search))

(global-set-key (kbd "C-c C-f") 'appleshan/open-readme-in-git-root-directory)

(global-set-key (kbd "<f5>") 'appleshan/run-current-file)

(spacemacs/set-leader-keys "gT" 'my-git-timemachine)

(spacemacs/set-leader-keys "bl" 'popwin:display-last-buffer)

;; layout
(spacemacs/set-leader-keys "l" nil)
(spacemacs/declare-prefix "l" "layout")
(spacemacs/set-leader-keys "ll" 'appleshan/load-my-layout)
(spacemacs/set-leader-keys "ls" 'appleshan/save-my-layout)

;; search
(define-key global-map (kbd "C-s") 'my-swiper-search)

;; java
; (define-key java-mode-map (kbd "M-i") 'java-imports-add-import-dwim)

; (global-set-key (kbd "C-h j") 'javadoc-lookup)
; (global-set-key [(f1)]      'javadoc-lookup)  ; F1 to lookup term on the configured Javadocs.
; (global-set-key [(meta f1)] 'javadoc-help)    ; meta-F1 to bring up the Javadoc-help menu to set up Javadocs.


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; keybindings.el ends here
