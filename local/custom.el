;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Emacs 主题设置 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil :height 130)
;; make the fringe thinner (default is 8 in pixels)
(set-fringe-mode '(8 . 8))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(helm-buffer-skip-remote-checking t)
 '(hl-paren-colors (quote ("Cyan" "Gold" "Red")))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(paradox-github-token t)
 '(smiley-style (quote medium))
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#dddd00"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
 '(ac-menu-face ((t (:background "Grey10" :foreground "Grey40"))))
 '(ac-selection-face ((t (:background "darkred" :foreground "grey"))))
 '(ac-yasnippet-candidate-face ((t (:background "#191919" :foreground "#878787"))))
 '(ac-yasnippet-menu-face ((t (:background "Grey10" :foreground "Grey40"))))
 '(ac-yasnippet-selection-face ((t (:background "darkgreen" :foreground "Grey"))))
 '(ascii-ascii-face ((((class color) (background dark)) (:background "Black" :foreground "Grey"))))
 '(ascii-non-ascii-face ((((class color) (background dark)) (:background "Black" :foreground "Gold"))))
 '(col-highlight ((t (:background "Grey5"))))
 '(company-preview ((t (:background "gold3" :foreground "black"))))
 '(company-preview-common ((t (:background "gold3" :foreground "grey20"))))
 '(company-preview-search ((t (:background "green4" :foreground "green"))))
 '(company-tooltip ((t (:background "darkred" :foreground "grey"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :foreground "gold"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "gold"))))
 '(company-tooltip-selection ((default (:background "red3" :foreground "black")) (((class color) (min-colors 88)) (:background "orange1"))))
 '(completion-tooltip-face ((t (:inherit tooltip :background "grey5" :foreground "khaki1" :family "文泉驿等宽微米黑"))))
 '(completions-common-part ((t (:foreground "Green3"))))
 '(completions-first-difference ((t (:foreground "Grey60"))))
 '(cursor ((t (:background "red"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "gold"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "DodgerBlue"))))
 '(dired-header ((t (:inherit font-lock-type-face :foreground "gold"))))
 '(dired-ignored ((t (:inherit shadow :foreground "grey50"))))
 '(dired-symlink ((t (:inherit font-lock-keyword-face :foreground "OrangeRed3"))))
 '(eldoc-highlight-function-argument ((t (:inherit bold :foreground "Red"))))
 '(fixme-face ((t (:foreground "orange" :box (:line-width 1 :color "orange") :weight bold))))
 '(go-to-char-highlight ((((class color) (background dark)) (:background "Pink" :foreground "Black"))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "DarkRed" :foreground "White"))))
 '(highlight-cl ((t (:foreground "#20ABFC" :underline nil))))
 '(highlight-cl-and-other ((t (:foreground "#20ABFC" :underline nil))))
 '(highlight-cl-macro ((t (:underline nil))))
 '(highlight-symbol-face ((((class color) (background dark)) (:background "grey" :foreground "white"))))
 '(hl-line ((t (:background "grey5"))))
 '(hl-sexp-face ((((class color) (background dark)) (:background "gray2"))))
 '(info-elisp-command-ref-item ((t (:background "Black" :foreground "yellow3"))))
 '(info-elisp-function-ref-item ((t (:background "Black" :foreground "Gold3"))))
 '(info-elisp-macro-ref-item ((t (:background "Black" :foreground "Yellow3"))))
 '(info-elisp-reference-item ((t (:background "Black" :foreground "DarkRed"))))
 '(info-elisp-special-form-ref-item ((t (:background "Black" :foreground "OrangeRed2"))))
 '(info-elisp-syntax-class-item ((t (:background "Black" :foreground "Khaki3"))))
 '(info-elisp-user-option-ref-item ((t (:background "Black" :foreground "LawnGreen"))))
 '(info-elisp-variable-ref-item ((t (:background "Black" :foreground "#0048FF"))))
 '(info-file ((t (:background "Black" :foreground "Blue"))))
 '(info-menu ((t (:foreground "DarkRed"))))
 '(info-menu-header ((t (:inherit variable-pitch :foreground "khaki3" :weight bold))))
 '(info-quoted-name ((t (:foreground "Purple"))))
 '(info-string ((t (:foreground "Grey50"))))
 '(info-title-1 ((t (:inherit info-title-2 :foreground "Gold" :height 1.1))))
 '(info-title-2 ((t (:inherit info-title-3 :foreground "red" :height 1.1))))
 '(info-title-3 ((t (:inherit info-title-4 :foreground "DodgerBlue" :height 1.1))))
 '(info-title-4 ((t (:inherit variable-pitch :foreground "Green" :weight bold))))
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "brown" :foreground "white"))))
 '(isearch-fail ((((class color) (min-colors 88) (background dark)) (:background "red4" :foreground "white"))))
 '(italic ((t (:underline nil :slant normal))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "grey20"))))
 '(match ((((class color) (min-colors 88) (background dark)) (:background "Black" :foreground "Grey70" :weight extra-bold))))
 '(message-header-subject ((t (:foreground "gold" :weight bold))))
 '(message-header-to ((t (:foreground "DarkRed" :weight bold))))
 '(minibuffer-prompt ((((background dark)) (:foreground "green"))))

;;; Date
;; Date: Saturday   27 July 2013
'(org-date ((t (:foreground "gray"
                :background "#000f0a"
                :box '(:color "black" :line-width 1 :style nil)
                :underline nil))))
'(org-agenda-date ((t (
                    :foreground "white" :background "#004A5D"
                    :box '(:color "dark cyan" :line-width 3 :style nil)
                    :weight bold))))
'(org-agenda-date-today ((t (
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 5 :style nil)
                    :weight bold))))
'(org-agenda-date-weekend ((t (
                    :foreground "deep pink"
                    :background "#222222"
                    :box '(:color "dark cyan" :line-width 3 :style nil)
                    :weight bold))))
'(org-agenda-current-time ((t (
                    :foreground "cyan" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :bold nil))))

;; Daily entry (holidays)
'(org-agenda-diary ((t (:foreground "light blue" :slant italic))))

;; clocking
'(org-clock-overlay ((t (:inverse-video nil
                    :foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil)
                    :bold t))))
'(org-agenda-clocking ((t (:foreground "white" :background "#004A5D"
                    :box '(:color "cyan" :line-width 1 :style nil)))))

;; Day-agenda (W30) -> Week number
'(org-agenda-structure ((t (:foreground "blue" :weight extra-bold))))
'(org-agenda-filter-tags ((t (:foreground "green yellow"))))
'(org-agenda-dimmed-todo-face ((t (:foreground "#444444"
                    :background "#222222"
                    :strike-through t))))

;; DONE (org agenda log state change tasks, )
'(org-agenda-done ((t (:foreground "#444444"
                    :background "black"
                    :height 1.0))))

;;; Agenda Time Grid
;; time grid: 18:00 ...... ----------------
'(org-time-grid ((t (:foreground "cyan"))))
;; alread past deadline in agenda
'(org-warning ((t (:foreground "red"
                    :weight normal))))
;; comming deadline in agenda
'(org-upcoming-deadline ((t (:foreground "OrangeRed"))))
;; scheduled in agenda, scheduled today, & org-habit
'(org-scheduled-today ((t (:foreground "light sea green" :height 1.0))))
'(org-scheduled ((t (:foreground "forest green"))))
'(org-scheduled-previously ((t (:foreground "olive drab"))))

;; Emphasize
;;; org-verbatim: =code=
'(org-verbatim ((t (:background "#004A5D" :foreground "white"
                    :box '(:color "cyan" :line-width 1)
                    :underline nil))))

;; table
;; '(org-table ((t (:foreground "dodger blue" :background "#001e15"))))

;;; headline faces
;;; the ahead stars face when org indentation. (org-hide)
'(org-hide ((t (:foreground "#002B36" :background "#002B36"))))
'(org-document-title ((t (:inherit nil
                    :weight bold
                    :height 1.5))))
'(org-level-1 ((t (:inherit nil
                    :family "DejaVu Sans Mono"
                    :weight bold :height 1.3
                    :foreground "Steelblue4"
                    :background "#001e15"
                    ;; :box '(:color "black" :line-width -1 :style nil)
                    :overline "dark slate gray"))))
'(org-level-2 ((t (:inherit 'org-level-1
                    :foreground "yellow4"))))
'(org-level-3 ((t (:inherit 'org-level-2
                    :foreground "#009E00"))))
'(org-level-4 ((t (:inherit 'org-level-3
                    :foreground "cyan"))))
'(org-level-5 ((t (:inherit 'org-level-4
                    :foreground "#008080"))))
'(org-level-6 ((t (:inherit 'org-level-5
                    :foreground "#166DEF"))))
'(org-level-7 ((t (:inherit 'org-level-6
                    :foreground "deep sky blue"))))
'(org-level-8 ((t (:inherit 'org-level-7
                    :foreground "chocolate"))))
'(org-headline-done ((t (:foreground "#444444"))))

;; ellipsis
;; (setq org-ellipsis "...⤵")
'(org-ellipsis ((t (:foreground "red"
                    :weight bold
                    :underline nil))))

;;; tags
'(org-tag ((t (:foreground "cyan"
                    :underline nil :weight normal :slant normal
                    :box '(:color "dark green" :line-width 2)
                    ;; :height 0.8
))))

;; meta lines
'(org-meta-line ((t (:foreground "yellow"
                    :background "#000f0a"))))

;;; checkbox faces
;; - [ ], - [X]
'(org-checkbox ((t (:bold normal
                    :box '(:line-width 1 :color "black" :style nil)
                    :foreground "dark gray"))))

;; * headline [7%] -> checkbox statistics face.
'(org-checkbox-statistics-todo ((t (:box '(:color "black" :line-width -1)
                    :foreground "green yellow"
                    :background "#000f0a"))))
'(org-checkbox-statistics-done ((t (:background "#444444" :foreground "black"
                    :box '(:color "black" :line-width -1)
                    :strike-through t))))

;;; list definition terms
'(org-list-dt ((t (:foreground "#444444"))))

;;; link face
'(org-link ((t (:foreground "cyan"
                    :background "#000f0a"
                    :underline "dark cyan"
                    ;; :box '(:color "black")
))))

;; <<target link>>
'(org-target ((t (:foreground "orange" :background "black"
                    :underline "red"
                    :weight bold))))

;; org structure faces
'(org-agenda-structure ((t (:foreground "gray" :weight bold))))

;; set Org clock face.
;; That is, make the org-mode-line-clock no longer inherit attributes from the
;; mode-line face. It seems like it gets the attributes from mode-line or
;; mode-line-inactive as appropriate, when displayed in the mode line.
'(org-mode-line-clock ((t (:foreground "cyan" :inherit nil))))

;; special keywords
'(org-special-keyword ((t (:foreground "forest green"
                    :background "#001912"))))

;; property
'(org-property-value ((t (:foreground "gray"))))

;;; Babel, Source Code, Block

;;; black style code block colorscheme
;;; babel faces (source code block) => #+BEGIN_SRC ... #+END_SRC
'(org-block-begin-line ((t (:foreground "dark cyan"
                    :background "#001912"
                    :weight normal :slant normal
                    :box '(:color "black" :line-width 1)))))
'(org-block-end-line ((t (:foreground "dark cyan"
                    :background "#001912"
                    :weight normal :slant normal
                    :box '(:color "black" :line-width 1)))))

'(secondary-selection ((t (:background "#000a07"))))


;; code face => ~code~,  #+RESULTS: : result.
'(org-code ((t (:background "#222222" :foreground "orange"
                    ;; :box '(:color "cyan" :line-width 1 :style nil)
                    ;; :underline '(:color "cyan") :box nil
                    :family "Source Code Pro"
                    :bold nil :box nil))))

;;; Formula face
'(org-formula ((t (:background "green yellow"
                    :foreground "black"
                    :inverse-video nil
                    :box '(:color "green yellow" :line-width 1 :style nil)))))

 '(region ((((class color) (min-colors 88) (background dark)) (:background "green4" :foreground "black"))))
 '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "Black"))))
 '(show-paren-match ((((class color) (background dark)) (:background "green" :foreground "black"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white"))))
 '(showtip-face ((((class color)) (:inherit tooltip :background "#730D0D" :foreground "White" :height 1.0 :family "文泉驿等宽微米黑"))))
 '(tooltip ((((class color)) (:inherit variable-pitch :background "DarkRed" :foreground "White" :family "文泉驿等宽微米黑"))))
 '(which-func ((((class color) (min-colors 88) (background dark)) (:foreground "Yellow"))))
 '(whitespace-highlight ((((class color) (background dark)) (:background "yellow2" :foreground "black"))))
 '(yas/field-highlight-face ((t (:background "grey20" :foreground "gold"))))
 '(yas/mirror-highlight-face ((t (:background "brown" :foreground "white")))))
