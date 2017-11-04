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

;; @see https://emacs-china.org/t/better-default-c-e/1573/6
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

;; 逗号后面自动加空格
(global-set-key (kbd ",") #'(lambda () (interactive) (insert ", ")))

(spacemacs/declare-prefix "o" "org") ;; modify by appleshan
(spacemacs/set-leader-keys ;; modify by appleshan
  ;; org-agenda
  "o#" 'org-agenda-list-stuck-projects
  "o/" 'org-occur-in-agenda-files
  "oa" 'org-agenda-list
  "oe" 'org-store-agenda-views
  "om" 'org-tags-view
  "oo" 'org-agenda
  "os" 'org-search-view
  "ot" 'org-todo-list
  ;; other
  "oO" 'org-clock-out
  "oc" 'org-capture
  "ol" 'org-store-link)

;; Search (and search/replace) using regex by default, since that's usually what I want to do:
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

; (global-set-key (kbd "M-/") 'company-complete)

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

(spacemacs/set-leader-keys "bl" 'popwin:display-last-buffer)

;; layout
(spacemacs/set-leader-keys "l" nil)
(spacemacs/declare-prefix "l" "layout")
(spacemacs/set-leader-keys "ll" 'appleshan/load-my-layout)
(spacemacs/set-leader-keys "ls" 'appleshan/save-my-layout)

; (spacemacs/declare-prefix "aD" "database")
; (spacemacs/set-leader-keys "aDm" 'dbclient/sql-connect-server)

;; Enable navigation by visual lines
;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(global-set-key (kbd "C-c n") #'cleanup-buffer)

;; terminal-here
(global-set-key (kbd "C-<f5>") #'terminal-here-launch)
(global-set-key (kbd "C-<f6>") #'terminal-here-project-launch)

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
