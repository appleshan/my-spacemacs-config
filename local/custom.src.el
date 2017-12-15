;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Emacs 主题设置 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @see https://www.emacswiki.org/emacs/LazyCatTheme.el
(set-face-attribute 'default nil :height 130)
;; make the fringe thinner (default is 8 in pixels)
;; make fringe mini size
(set-fringe-mode '(1 . 1))

;; 用户自定义变量
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(exec-path-from-shell-arguments (quote ("-l")))
 '(helm-buffer-skip-remote-checking t)
 ; highlight-parentheses : 高亮括号结构
 '(hl-paren-colors (quote ("Cyan"       ;一级颜色
                           "Gold"       ;二级颜色
                           "Red"        ;三级颜色
                           )))
 '(smiley-style (quote medium)) ;笑脸的风格, 中等, 10种颜色
 ; '(tabbar-background-color "black")
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#dddd00"))

;; 用户自定义外观
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; Auto-complete
 '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
 '(ac-menu-face ((t (:background "Grey10" :foreground "Grey40")))) ;菜单颜色
 '(ac-selection-face ((t (:background "darkred" :foreground "grey")))) ;选择颜色
 '(ac-yasnippet-candidate-face ((t (:background "#191919" :foreground "#878787"))))
 '(ac-yasnippet-menu-face ((t (:background "Grey10" :foreground "Grey40")))) ;Yasnippet 菜单颜色
 '(ac-yasnippet-selection-face ((t (:background "darkgreen" :foreground "Grey")))) ;Yasnippet 选择颜色
 ;; Ascii
 '(ascii-ascii-face ((((class color) (background dark)) (:background "Black" :foreground "Grey")))) ;ascii字符的编码
 '(ascii-non-ascii-face ((((class color) (background dark)) (:background "Black" :foreground "Gold")))) ;Non-ascii字符的编码
 ;; col-highlight
 '(col-highlight ((t (:background "Grey5")))) ;当前列的高亮背景色
 ;; Company 跟主题一样的风格
 '(company-tooltip ((t (:inherit 'mode-line))))
 '(company-scrollbar-bg ((t (:inherit 'mode-line-inactive))))
 '(company-scrollbar-fg ((t (:inherit 'tooltip))))
 '(company-tooltip-selection ((t (:inherit 'highlight))))
 '(company-tooltip-common ((t (:inherit 'mode-line))))
 '(company-tooltip-common-completion ((t (:inherit 'mode-line))))
 '(company-tooltip-annotation ((t (:inherit 'mode-line))))
 ;; Completion ui
 '(completion-tooltip-face ((t (:inherit tooltip :background "grey5" :foreground "khaki1" :family "文泉驿等宽微米黑"))))
 '(completions-common-part ((t (:foreground "Green3")))) ;补全相同部分
 '(completions-first-difference ((t (:foreground "Grey60")))) ;补全不同部分
 '(cursor ((t (:background "red"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "gold"))))
 ;; Dired
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
 ;; eldoc
 '(eldoc-highlight-function-argument ((t (:inherit bold :foreground "Red")))) ;参数颜色
 ; '(erc-direct-msg-face ((t (:foreground "DodgerBlue"))))
 ; '(erc-input-face ((t (:foreground "Green2"))))
 ; '(erc-my-nick-face ((t (:foreground "DarkRed" :weight bold))))
 ; '(erc-notice-face ((t (:foreground "Gray20" :weight bold))))
 ; '(erc-prompt-face ((t (:background "Black" :foreground "Gold" :weight bold))))
 ;; Highlight fixme
 '(fixme-face ((t (:foreground "orange" :box (:line-width 1 :color "orange") :weight bold))))
 ;; 窗口边缘
 '(fringe ((((class color) (background dark)) (:background "gray3"))))
 ; '(gnus-button ((t (:foreground "khaki3" :weight bold))))
 ; '(gnus-cite-1 ((((class color) (background dark)) (:foreground "Grey50"))))
 ; '(gnus-header-content ((t (:foreground "Green" :slant italic))))
 ; '(gnus-header-from ((((class color) (background dark)) (:foreground "khaki"))))
 ; '(gnus-header-name ((((class color) (background dark)) (:foreground "DodgerBlue"))))
 ; '(gnus-header-subject ((((class color) (background dark)) (:foreground "HotPink"))))
 ; '(gnus-signature ((t (:foreground "Orange" :slant italic))))
 ; '(gnus-summary-high-ancient ((t (:foreground "Grey50" :weight bold))))
 ; '(gnus-summary-high-read ((t (:foreground "Gold2" :weight bold))))
 ; '(gnus-summary-low-ancient ((t (:foreground "Grey10" :slant italic))))
 ; '(gnus-summary-low-read ((t (:foreground "Gold4" :slant italic))))
 ; '(gnus-summary-normal-ancient ((((class color) (background dark)) (:foreground "Grey40"))))
 ; '(gnus-summary-normal-read ((((class color) (background dark)) (:foreground "khaki2"))))
 ;; Go-to-char
 '(go-to-char-highlight ((((class color) (background dark)) (:background "Pink" :foreground "Black")))) ;跳转到字符高亮
 ;; Highlight
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "DarkRed" :foreground "White"))))
 ;; 高亮 CL 函数
 '(highlight-cl ((t (:foreground "#20ABFC" :underline nil)))) ;`cl' 函数
 '(highlight-cl-and-other ((t (:foreground "#20ABFC" :underline nil)))) ;`cl' 函数， 但是定义在其他包
 '(highlight-cl-macro ((t (:underline nil)))) ;`cl' 宏
 ;; hl-line+
 '(hl-line ((t (:background "grey5")))) ;当前行高亮背景色
 ;; hl-sexp
 '(hl-sexp-face ((((class color) (background dark)) (:background "gray2")))) ;高亮 sexp
 ;; Info
 '(info-elisp-command-ref-item ((t (:background "Black" :foreground "yellow3")))) ;elisp命令引用项目
 '(info-elisp-function-ref-item ((t (:background "Black" :foreground "Gold3")))) ;elisp函数引用项目
 '(info-elisp-macro-ref-item ((t (:background "Black" :foreground "Yellow3")))) ;elisp宏引用项目
 '(info-elisp-reference-item ((t (:background "Black" :foreground "DarkRed")))) ;elisp引用项目
 '(info-elisp-special-form-ref-item ((t (:background "Black" :foreground "OrangeRed2")))) ;elisp特殊表格引用项目
 '(info-elisp-syntax-class-item ((t (:background "Black" :foreground "Khaki3")))) ;elisp语法类型项目
 '(info-elisp-user-option-ref-item ((t (:background "Black" :foreground "LawnGreen")))) ;elisp用户选项引用项目
 '(info-elisp-variable-ref-item ((t (:background "Black" :foreground "#0048FF")))) ;elisp变量引用项目
 '(info-file ((t (:background "Black" :foreground "Blue")))) ;文件
 '(info-menu ((t (:foreground "DarkRed")))) ;菜单
 '(info-menu-header ((t (:inherit variable-pitch :foreground "khaki3" :weight bold)))) ;菜单标题
 '(info-quoted-name ((t (:foreground "Purple")))) ;引用名字
 '(info-string ((t (:foreground "Grey50")))) ;字符串
 '(info-title-1 ((t (:inherit info-title-2 :foreground "Gold" :height 1.1)))) ;标题1
 '(info-title-2 ((t (:inherit info-title-3 :foreground "red" :height 1.1)))) ;标题2
 '(info-title-3 ((t (:inherit info-title-4 :foreground "DodgerBlue" :height 1.1)))) ;标题3
 '(info-title-4 ((t (:inherit variable-pitch :foreground "Green" :weight bold)))) ;标题4
 ;; Isearch
 '(isearch ((((class color) (min-colors 88) (background dark)) (:background "brown" :foreground "white")))) ;搜索关键字
 '(isearch-fail ((((class color) (min-colors 88) (background dark)) (:background "red4" :foreground "white")))) ;搜索失败
 '(italic ((t (:underline nil :slant normal))))
 ; '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "grey20"))))
 ;; Match
 '(match ((((class color) (min-colors 88) (background dark)) (:background "Black" :foreground "Grey70" :weight extra-bold)))) ;匹配的
 '(message-header-subject ((t (:foreground "gold" :weight bold))))
 '(message-header-to ((t (:foreground "DarkRed" :weight bold))))
 ;; Minibuffer
 '(minibuffer-prompt ((((background dark)) (:foreground "green")))) ;提示
 ;; Org-mode

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
'(org-table ((t (:foreground "dodger blue" :background "#001e15"))))

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
                    ;; :box '(:color "black")))))

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
'(org-mode-line-clock ((t (:foreground "cyan"
                    :inherit nil))))

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

; '(org-date ((((class color) (background dark)) (:foreground "ivory4" :underline t)))) ;日期
 '(org-ellipsis ((((class color) (background dark)) (:background "black" :foreground "Cyan" :strike-through nil)))) ;省略号
 '(org-hide ((((background dark)) (:foreground "black")))) ;隐藏星号
 '(org-level-3 ((t (:inherit outline-3 :foreground "DeepSkyBlue"))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "VioletRed3"))))
 '(org-level-6 ((t (:inherit outline-6 :foreground "violet"))))
 '(org-level-7 ((t (:inherit outline-7 :foreground "khaki3"))))
 '(org-level-8 ((t (:inherit outline-8 :foreground "DarkSeaGreen"))))
 '(org-link ((((class color) (background dark)) (:foreground "Cyan"))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t)
 '(org-special-keyword ((((class color) (min-colors 16) (background dark)) (:foreground "rosybrown1")))) ;关键字
 '(org-todo ((t (:foreground "Red" :weight bold))))
 ; '(popup-isearch-match ((t (:background "#191919" :foreground "#ffffff"))))
 ; '(popup-menu-mouse-face ((t (:background "gold" :foreground "white"))))
 ; '(popup-menu-summary-face ((t (:background "#191919" :foreground "grey"))))
 ; '(popup-scroll-bar-background-face ((t (:background "#191919"))))
 ; '(popup-scroll-bar-foreground-face ((t (:background "#393939"))))
 ; '(popup-summary-face ((t (:background "#191919" :foreground "grey"))))
 ;选中区域的颜色
 '(region ((((class color) (min-colors 88) (background dark)) (:background "green4" :foreground "black"))))
 '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "Black")))) ;次要级的选择
 ;; 语法高亮
 '(show-paren-match ((((class color) (background dark)) (:background "green" :foreground "black")))) ;括号匹配
 '(show-paren-mismatch ((((class color)) (:background "red" :foreground "white")))) ;括号没有匹配
 ;; Showtip
 '(showtip-face ((((class color)) (:inherit tooltip :background "#730D0D" :foreground "White" :height 1.0 :family "文泉驿等宽微米黑"))))
 ;; Tooltip
 '(tooltip ((((class color)) (:inherit variable-pitch :background "DarkRed" :foreground "White" :family "文泉驿等宽微米黑"))))
 ; '(top-mode-mark-face (quote isearch))
 ;当前函数
 '(which-func ((((class color) (min-colors 88) (background dark)) (:foreground "Yellow"))))
 ;; Whitespace
 '(whitespace-highlight ((((class color) (background dark)) (:background "yellow2" :foreground "black")))) ;空格
 ;; Yasnippet
 '(yas/field-highlight-face ((t (:background "grey20" :foreground "gold")))) ;模版区域
 '(yas/mirror-highlight-face ((t (:background "brown" :foreground "white")))) ;同步模版区域
 )
