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

;; dired
;; @see https://github.com/abo-abo/hydra/wiki/Dired
(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)

;; ibuffer
;; @see https://github.com/abo-abo/hydra/wiki/Ibuffer
(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_: mark      _D_: delete       _g_: refresh    _q_: quit             _k_:   ↑    _h_
_u_: unmark    _s_: save marked  _S_: sort       _TAB_: toggle         _RET_: visit
_*_: specific  _a_: all actions  _/_: filter     _o_: other window     _j_:   ↓    _l_
_t_: toggle    _._: toggle hydra _H_: help       C-o other win no-select
"
  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)
  ("t" ibuffer-toggle-marks)

  ("D" ibuffer-do-delete)
  ("s" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("S" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)
  ("H" describe-mode :color blue)

  ("h" ibuffer-backward-filter-group)
  ("k" ibuffer-backward-line)
  ("l" ibuffer-forward-filter-group)
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)

  ("TAB" ibuffer-toggle-filter-group)

  ("o" ibuffer-visit-buffer-other-window :color blue)
  ("q" quit-window :color blue)
  ("." nil :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

;; Automatically open the hydra with Ibuffer.
(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)

;; Org mode block templates
;; @see https://github.com/abo-abo/hydra/wiki/Org-mode-block-templates
(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter   _N_AME      _e_macs-lisp    _i_ndex:
_v_erse    _l_atex     _S_cheme        _L_aTeX:
qu_o_te    _h_tml      _p_ython        _H_TML:
_E_xample  ^ ^         _s_hell         _I_NCLUDE:
^ ^        ^ ^         plant_u_ml      ^ ^:
"
  ("c" (hot-expand "<c"))
  ("v" (hot-expand "<v"))
  ("o" (hot-expand "<q"))
  ("E" (hot-expand "<e"))
  ("N" (hot-expand "<N"))
  ("l" (hot-expand "<l"))
  ("a" (hot-expand "<a"))
  ("h" (hot-expand "<h"))
  ("e" (hot-expand "<s" "emacs-lisp"))
  ("S" (hot-expand "<s" "scheme"))
  ("p" (hot-expand "<s" "python"))
  ("s" (hot-expand "<s" "sh"))
  ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
  ("i" (hot-expand "<i"))
  ("L" (hot-expand "<L"))
  ("A" (hot-expand "<A"))
  ("H" (hot-expand "<H"))
  ("I" (hot-expand "<I"))
  ("<" self-insert-command "ins")
  ("q" nil "quit"))

  (defun hot-expand (str &optional mod header)
    "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end))
        (deactivate-mark))
      (when header (insert "#+HEADER: " header) (forward-line))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

(with-eval-after-load 'org
  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1)))))

(defhydra hydra-global-org (:color blue)
  "Org"
  ("t" org-timer-start "Start Timer")
  ("s" org-timer-stop "Stop Timer")
  ("r" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
  ("p" org-timer "Print Timer") ; output timer value to buffer
  ("w" (org-clock-in '(4)) "Clock-In") ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
  ("o" org-clock-out "Clock-Out") ; you might also want (setq org-log-note-clock-out t)
  ("j" org-clock-goto "Clock Goto") ; global visit the clocked task
  ("c" org-capture "Capture") ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
  ("l" org-capture-goto-last-stored "Last Capture"))

;; symbol-overlay
(defhydra hydra-symbol-overlay (:hint nil)
  "
[_P_]ut       Jump to [_n_]ext        Search [_l_]iterally
[_T_]oggle            [_p_]revious     ...or [_Q_]uery Replace
[_e_]cho              [_d_]efinition   ...or [_R_]ename
[_S_]ave
"
  ("P" symbol-overlay-put)
  ("n" symbol-overlay-jump-next)
  ("p" symbol-overlay-jump-prev)
  ("S" symbol-overlay-save-symbol)
  ("T" symbol-overlay-toggle-in-scope)
  ("e" symbol-overlay-echo-mark)
  ("d" symbol-overlay-jump-to-definition)
  ("l" symbol-overlay-isearch-literally)
  ("Q" symbol-overlay-query-replace)
  ("R" symbol-overlay-rename)
  ("q" nil :color blue))

(spacemacs/set-leader-keys "os" #'hydra-symbol-overlay/body)

;; dumb-jump
(defhydra hydra-dumb-jump (:color amaranth)
  "Dumb Jump"
  ("g" dumb-jump-go "Go")
  ("b" dumb-jump-back "Back")
  ("l" dumb-jump-quick-look "Look")
  ("e" dumb-jump-go-prefer-external-other-window "External" :color blue)
  ("w" dumb-jump-go-other-window "Window" :color blue)
  ("p" dumb-jump-go-prompt "Prompt")
  ("q" nil "Quit" :color blue))

(global-set-key (kbd "C-j") #'hydra-dumb-jump/body)

;; emacs-lisp
(defhydra hydra-lisp-eval (:color blue :columns 2 :idle 1.0)
  "Lisp Eval"
  ("r" eval-region "Region")
  ("b" eval-buffer "Buffer")
  ("e" eval-expression "S-expression")
  ("l" eval-last-sexp "Last S-expression")
  ("L" eval-print-last-sexp "Last S-expression and Print Value")
  ("d" eval-defun "Defun / Function")
  ("f" eval-defun "Defun / Function"))

(with-eval-after-load 'emacs-lisp
  (bind-key "C-c C-e" #'hydra-lisp-eval/body emacs-lisp-mode-map)
  (bind-key "C-c C-e" #'hydra-lisp-eval/body lisp-mode-map))

;; quickrun
(defhydra hydra-quickrun (:color blue)
  "Quickrun"
  ("q" quickrun "run")
  ("r" quickrun-region "region")
  ("w" quickrun-with-arg "with-arg")
  ("s" quickrun-shell "shell")
  ("c" quickrun-compile-only "compile")
  ("p" quickrun-replace-region "replace"))

;; wandbox
(defhydra hydra-wandbox (:color amaranth :hint nil)
  "
Wandbox
————————————————————————————————————————
Compile: _F_ile     _L_ist Compilers
         _B_uffer   _I_nsert Template
         _R_egion
"
  ("F" wandbox-compile-file)
  ("B" wandbox-compile-buffer)
  ("R" wandbox-compile-region)
  ("L" wandbox-list-compilers :color red)
  ("I" wandbox-insert-template)
  ("q" nil))

(with-eval-after-load 'wandbox
  (bind-key "w" #'hydra-wandbox/body prog-mode-map))

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))

(defhydra hydra-undo-tree (:hint nil)
  "
  _p_: undo  _n_: redo _s_: save _l_: load   "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("u"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue))

(global-set-key (kbd "C-x u") 'hydra-undo-tree/undo-tree-undo)`

(defhydra hydra-pdftools (:color blue :hint nil)
        "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
        ("\\" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("al" pdf-annot-list-annotations)
        ("ad" pdf-annot-delete)
        ("aa" pdf-annot-attachment-dired)
        ("am" pdf-annot-add-markup-annotation)
        ("at" pdf-annot-add-text-annotation)
        ("y"  pdf-view-kill-ring-save)
        ("+" pdf-view-enlarge :color red)
        ("-" pdf-view-shrink :color red)
        ("0" pdf-view-scale-reset)
        ("H" pdf-view-fit-height-to-window)
        ("W" pdf-view-fit-width-to-window)
        ("P" pdf-view-fit-page-to-window)
        ("n" pdf-view-next-page-command :color red)
        ("p" pdf-view-previous-page-command :color red)
        ("d" pdf-view-dark-minor-mode)
        ("b" pdf-view-set-slice-from-bounding-box)
        ("r" pdf-view-reset-slice)
        ("g" pdf-view-first-page)
        ("G" pdf-view-last-page)
        ("e" pdf-view-goto-page)
        ("o" pdf-outline)
        ("s" pdf-occur)
        ("i" pdf-misc-display-metadata)
        ("u" pdf-view-revert-buffer)
        ("F" pdf-links-action-perfom)
        ("f" pdf-links-isearch-link)
        ("B" pdf-history-backward :color red)
        ("N" pdf-history-forward :color red)
        ("l" image-forward-hscroll :color red)
        ("h" image-backward-hscroll :color red))

(defhydra hydra-smerge
  (:color red :hint nil
          :pre (smerge-mode 1))
  "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase _R_efine _<_: base-mine
_p_rev _m_ine _E_diff _=_: mine-other
^ ^ _o_ther _C_ombine _>_: base-other
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
"
  ("RET" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("n" smerge-next)
  ("o" smerge-keep-other)
  ("p" smerge-prev)
  ("r" smerge-resolve)
  ("<" smerge-diff-base-mine)
  ("=" smerge-diff-mine-other)
  (">" smerge-diff-base-other)
  ("q" nil :color blue))


;; @see https://emacs-china.org/t/better-default-c-e/1573/6
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

;; 逗号后面自动加空格
(global-set-key (kbd ",") #'(lambda () (interactive) (insert ", ")))

(spacemacs/set-leader-keys ;; modify by appleshan
  ;; Go to next org file in org-agenda-files
  "aoC"  'org-cycle-agenda-files

  ;; save
  "aoS"  'org-save-all-org-buffers

  ;; toggle
  "aoTb" 'org-hide-block-toggle-all
  "aoTi" 'org-toggle-inline-images
  "aoTl" 'org-toggle-link-display
  )

;; Search (and search/replace) using regex by default, since that's usually what I want to do:
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)

; (global-set-key (kbd "M-/") 'company-complete)

(global-set-key (kbd "C-s-\\") 'appleshan/evil-toggle-input-method)

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

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

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
