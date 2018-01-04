;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Emacs 主题设置 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil :height 130)
;; make the fringe thinner (default is 8 in pixels)
(set-fringe-mode '(16 . 16))

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
 '(company-tooltip ((t (:inherit 'mode-line))))
 '(company-scrollbar-bg ((t (:inherit 'mode-line-inactive))))
 '(company-scrollbar-fg ((t (:inherit 'tooltip))))
 '(company-tooltip-selection ((t (:inherit 'highlight))))
 '(company-tooltip-common ((t (:inherit 'mode-line))))
 '(company-tooltip-common-completion ((t (:inherit 'mode-line))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-annotation ((t (:inherit 'mode-line))))
 '(completion-dynamic-face ((((class color) (background dark)) (:background "DarkOrange" :foreground "black"))))
 '(completion-tooltip-face ((t (:inherit tooltip :background "grey5" :foreground "khaki1" :family "文泉驿等宽微米黑"))))
 '(completions-common-part ((t (:foreground "Green3"))))
 '(completions-first-difference ((t (:foreground "Grey60"))))
 '(cursor ((t (:background "red"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "gold"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "DodgerBlue"))))
 '(dired-header ((t (:inherit font-lock-type-face :foreground "gold"))))
 '(dired-ignored ((t (:inherit shadow :foreground "grey50"))))
 '(dired-symlink ((t (:inherit font-lock-keyword-face :foreground "OrangeRed3"))))
 '(diredp-date-time ((t (:foreground "Grey60")))) ;修改时间
 '(diredp-deletion ((t (:background "Black" :foreground "red")))) ;删除标记
 '(diredp-deletion-file-name ((t (:foreground "red")))) ;删除文件
 '(diredp-dir-heading ((t (:background "Black" :foreground "Gold")))) ;目录
 '(diredp-dir-priv ((t (:background "Black" :foreground "DodgerBlue")))) ;目录掩码
 '(diredp-display-msg ((t (:foreground "Gold")))) ;路径
 '(diredp-exec-priv ((t (:background "Black" :foreground "DeepSkyBlue3")))) ;可执行掩码
 '(diredp-file-name ((t (:foreground "Green3")))) ;文件
 '(diredp-file-suffix ((t (:foreground "Green4")))) ;文件扩展名
 '(diredp-flag-mark ((t (:background "Black" :foreground "Cyan")))) ;选中标记
 '(diredp-flag-mark-line ((t (:background "Black" :foreground "Cyan")))) ;选中文件
 '(diredp-ignored-file-name ((t (:foreground "grey40")))) ;忽略的文件
 '(diredp-no-priv ((t (:background "Black" :foreground "Green")))) ;无权限
 '(diredp-other-priv ((t (:background "Black" :foreground "khaki")))) ;其他权限
 '(diredp-rare-priv ((t (:background "Black" :foreground "Red")))) ;稀有的权限
 '(diredp-read-priv ((t (:background "Black" :foreground "IndianRed")))) ;读取权限
 '(diredp-write-priv ((t (:background "Black" :foreground "Gold3")))) ;写入权限
 '(eldoc-highlight-function-argument ((t (:inherit bold :foreground "Red"))))
 '(fixme-face ((t (:foreground "orange" :box (:line-width 1 :color "orange") :weight bold))))
 '(go-to-char-highlight ((((class color) (background dark)) (:background "Pink" :foreground "Black"))))
 '(hs-face ((t (:background "DarkRed" :foreground "grey" :box (:line-width 1 :color "grey50")))))
 '(hs-fringe-face ((t (:background "DarkRed" :foreground "grey" :box (:line-width 2 :color "grey75" :style released-button)))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "DarkRed" :foreground "White"))))
 '(highlight-cl ((t (:foreground "#20ABFC" :underline nil))))
 '(highlight-cl-and-other ((t (:foreground "#20ABFC" :underline nil))))
 '(highlight-cl-macro ((t (:underline nil))))
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
 '(region ((((class color) (min-colors 88) (background dark)) (:background "green4" :foreground "black"))))
 '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "Black"))))
 '(show-paren-match ((((class color) (background dark)) (:background "green" :foreground "black"))))
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white"))))
 '(showtip-face ((((class color)) (:inherit tooltip :background "#730D0D" :foreground "White" :height 1.0 :family "文泉驿等宽微米黑"))))
 '(tooltip ((((class color)) (:inherit variable-pitch :background "DarkRed" :foreground "White" :family "文泉驿等宽微米黑"))))
 '(woman-addition ((t (:foreground "Gold3"))))
 '(woman-bold ((((background dark)) (:foreground "Green3" :weight bold))))
 '(woman-italic ((((background dark)) (:foreground "DarkRed" :underline t))))
 '(woman-unknown ((((min-colors 88) (background dark)) (:foreground "Cyan3"))))
 '(which-func ((((class color) (min-colors 88) (background dark)) (:foreground "Yellow"))))
 '(whitespace-highlight ((((class color) (background dark)) (:background "yellow2" :foreground "black"))))
 '(yas/field-highlight-face ((t (:background "grey20" :foreground "gold"))))
 '(yas/mirror-highlight-face ((t (:background "brown" :foreground "white")))))
