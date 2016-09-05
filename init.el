;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '() ; "~/.spacemacs.d/layers/"
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; --- Auto Complete ---
     ivy           ; spacemacs develop branch
     ; spacemacs-ivy ; spacemacs master branch
     (auto-completion :variables
                      auto-completion-enable-help-tooltip nil ; 'manual
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t ; Adding yas-snippets
                      auto-completion-tab-key-behavior 'complete
                      :disabled-for org markdown)
     ;; auto-complete layer 在 orgmode 中会引发很多问题，所以在 org 中禁用 company 补全

     ;; --- UI ---
     (colors :variables
             colors-colorize-identifiers t
             colors-enable-nyan-cat-progress-bar nil)

     ;; --- Better Editor ---
     better-defaults

     ;; --- programming language layers ---
     emacs-lisp
     ; html
     ; javascript
     ; shell
     shell-scripts
     yaml

     ;; --- programming tool layers ---
     (dash :variables
           helm-dash-docset-newpath "~/.local/share/Zeal/docsets/")
     (git :variables
          git-magit-status-fullscreen t)
     github
     imenu-list
     prodigy  ;; 使用 Prodigy 在 Emacs 中管理外部服务
     (restclient :variables
                 restclient-use-org nil)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     version-control

     ;; --- Major Mode ---
     ; docker ; BUG: 
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     markdown
     org
     search-engine

     ;; ranger 与 vinegar 不能共存
     ; ranger
     (vinegar :variables
              vinegar-reuse-dired-buffer t)

     ;; --- Minor Mode ---
     (chinese :variables
              chinese-enable-fcitx t)
     ;; deft  ;; Quick Note Taking
     ; finance
     (spell-checking :variables
                     spell-checking-enable-by-default nil)

     ;; --- private layers ---
     appleshan
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be install and loaded.
   dotspacemacs-excluded-packages '(
                                    ; magit-gh-pulls
                                    ; magit-gitflow 
                                    ac-ispell
                                    ace-jump-helm-line
                                    ace-jump-mode
                                    ; ace-pinyin
                                    auto-complete
                                    auto-dictionary
                                    clean-aindent-mode
                                    company-quickhelp
                                    coffee-mode
                                    define-word
                                    evil-args
                                    evil-ediff
                                    ;; disable it for lispy-mode
                                    ;;https://github.com/abo-abo/lispy/issues/137
                                    ; evil-escape
                                    evil-exchange
                                    evil-indent-plus
                                    ; evil-lisp-state
                                    evil-mc
                                    evil-tutor
                                    evil-unimpaired
                                    ; eyebrowse
                                    fancy-battery
                                    ; find-by-pinyin-dired
                                    fish-mode
                                    flx-ido
                                    gh-md
                                    git-gutter
                                    git-gutter+
                                    git-gutter-fringe
                                    git-gutter-fringe+
                                    google-translate
                                    helm-company
                                    helm-c-yasnippet
                                    helm-make
                                    helm-mode-manager
                                    helm-projectile
                                    helm-pydoc
                                    helm-spacemacs-help
                                    helm-swoop
                                    helm-themes
                                    highlight-indentation
                                    hl-anything
                                    hydra
                                    ido-vertical-mode
                                    leuven-theme
                                    linum-relative
                                    livid-mode
                                    lorem-ipsum
                                    ; neotree
                                    open-junk-file
                                    orgit
                                    orglue
                                    org-download
                                    org-present
                                    org-projectile
                                    org-repo-todo
                                    org-timer
                                    rainbow-delimiters
                                    skewer-mode
                                    smartparens
                                    smeargle
                                    smex
                                    ;; smooth-scrolling
                                    spacemacs-theme
                                    spinner
                                    vi-tilde-fringe
                                    volatile-highlights)
   ;; Defines the behaviour of Spacemacs when downloading packages.
   ;; Possible values are `used', `used-but-keep-unused' and `all'. `used' will
   ;; download only explicitly used packages and remove any unused packages as
   ;; well as their dependencies. `used-but-keep-unused' will download only the
   ;; used packages but won't delete them if they become unused. `all' will
   ;; download all the packages regardless if they are used or not and packages
   ;; won't be deleted by Spacemacs. (default is `used')
   dotspacemacs-download-packages 'used))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 10
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of of
   ;; the form `(list-type . list-size)`. If nil it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 10))
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location nil
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 1
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers `prog-mode
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; activate debugging
  ; (setq debug-on-error t
  ;       debug-on-signal nil
  ;       debug-on-quit nil)

  ; (defvar stack-trace-on-error t)

  (setq configuration-layer--elpa-archives
        '(;("melpa-stable-cn" . "http://elpa.zilongshanren.com/melpa-stable/")
          ("melpa-cn" . "http://elpa.zilongshanren.com/melpa/")
          ("org-cn"   . "http://elpa.zilongshanren.com/org/") ; Org-mode's repository
          ("gnu-cn"   . "http://elpa.zilongshanren.com/gnu/")
          ))

  ;; Dropbox directory
  (defconst user-dropbox-directory
    (expand-file-name (concat user-home-directory "Dropbox/"))
    "Dropbox directory.")
  (unless (file-exists-p user-dropbox-directory)
    (make-directory user-dropbox-directory))

  ;; custom logo
  (setq spacemacs-banner-official-png
    (expand-file-name (concat dotspacemacs-directory "local/005-banner.txt")))

  ;; BUG : https://github.com/syl20bnr/spacemacs/issues/2705
  (setq tramp-ssh-controlmaster-options
    "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (setq yas-snippet-dirs nil)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (spacemacs/declare-prefix "d" "dash/zeal")
  (spacemacs/declare-prefix "fd" "dir")

  ;; {{ 解决 org 表格里面中英文对齐的问题
  ;; 字体大小：[ 12 14 ], [ 13 16 ]
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-linux) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "文泉驿等宽微米黑" 12 14)))
  ;; }}

  (setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\|192.168.*\\|elpa.zilongshanren.com\\)")
     ("http" . "127.0.0.1:18080")
     ("https" . "127.0.0.1:18080")
     ("socks5" . "127.0.0.1:18080")))

  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish spacemacs-whitespace-cleanup-mode)

  ;; restore the desktop
  (desktop-save-mode t)
  (desktop-read))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (git-gutter dumb-jump dockerfile-mode docker tablist docker-tramp javadoc-lookup mvn ledger-mode flycheck-ledger imenu-list xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help helm-gitignore helm-flyspell packed tiny flyspell-correct-ivy flyspell-correct discover-my-major auto-dictionary f py-autopep8 elpy find-file-in-project popup spinner s hydra parent-mode flx iedit anzu evil goto-chg undo-tree highlight diminish projectile pkg-info epl bind-map bind-key async package-build restclient ob-http smartparens avy dash amd-mode makey helm puml-mode vdiff counsel swiper helm-core ivy ht peep-dired powerline visual-regexp-steroids visual-regexp stickyfunc-enhance srefactor zeal-at-point yaml-mode web-beautify tldr super-save smeargle rainbow-mode rainbow-identifiers pyvenv pytest pyenv-mode py-yapf prodigy pip-requirements paredit pangu-spacing org-projectile org-pomodoro alert log4e gntp org-password-manager org org-download mwim mmm-mode markdown-toc markdown-mode magit-gitflow magit-gh-pulls livid-mode skewer-mode simple-httpd live-py-mode lispy json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc ibuffer-projectile hy-mode htmlize highlight-escape-sequences gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link fringe-helper gist gh marshal logito pcache flycheck-pos-tip flycheck-package flycheck find-by-pinyin-dired fcitx evil-magit magit magit-popup git-commit with-editor engine-mode emoji-cheat-sheet-plus dired-sort dired-k dired+ diff-hl cython-mode counsel-dash helm-dash company-tern dash-functional tern company-statistics company-shell company-quickhelp company-emoji company-anaconda company color-identifiers-mode chinese-pyim chinese-pyim-basedict pos-tip calfw google-maps cal-china-x browse-kill-ring beacon seq bbdb-vcard bbdb auto-yasnippet yasnippet anaconda-mode pythonic ace-pinyin pinyinlib ace-jump-mode ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spacemacs-theme spaceline smex restart-emacs request quelpa popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav define-word counsel-projectile column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-6 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-7 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-3 :height 1.0))))
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; init.el ends here
