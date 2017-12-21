;;; packages.el --- appleshan-navigation layer packages file for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是 appleshan 个人专用的 emacs 配置文件，emacs 中文用户可以参考。

;;; License: GPLv3

;;; Code:

(setq appleshan-navigation-packages
      '(
        dumb-jump
        evil
        evil-escape
        treemacs
        workgroups2
        ))

;; List of packages to exclude.
(setq appleshan-navigation-excluded-packages '())

(defun appleshan-navigation/post-init-dumb-jump ()
  ;; If your project has multi-line method signatures you should use ag.
  (setq dumb-jump-force-searcher 'ag)
  (setq dumb-jump-prefer-searcher 'ag))

(defun appleshan-navigation/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    ;; change evil initial mode state
    (loop for (mode . state) in
          '((shell-mode . normal))
          do (evil-set-initial-state mode state))

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
    (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "s-b") 'backward-word)

    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)

    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

    (define-key evil-visual-state-map (kbd "C-r") 'appleshan/evil-quick-replace)

    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)

    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)

    (evil-define-key 'emacs term-raw-map (kbd "C-w") 'evil-delete-backward-word)

    (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
          evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
          evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
          evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))
    (setq evil-insert-state-cursor '("chartreuse3" box))
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
    ))

(defun appleshan-navigation/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

;; A tree layout file explorer
(defun appleshan-navigation/post-init-treemacs ()
  (with-eval-after-load 'treemacs
    (setq treemacs-show-hidden-files nil)
    ))

(defun appleshan-navigation/init-workgroups2 ()
  (use-package workgroups2
  :init (require 'workgroups2)
  :config
  (progn
    (setq wg-session-load-on-start t) ; default: (not (daemonp))

    ;; Change prefix key (before activating WG)
    (setq wg-prefix-key (kbd "C-c z"))

    ;; Change workgroups session file
    (setq wg-session-file "~/.emacs.d/.cache/.emacs_workgroups")

    ;; Set your own keyboard shortcuts to reload/save/switch WGs:
    ;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
    (global-set-key (kbd "<pause>")     'wg-reload-session)
    (global-set-key (kbd "C-S-<pause>") 'wg-save-session)
    (global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
    (global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

    ;; What to do on Emacs exit / workgroups-mode exit?
    (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
    (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

    ;; Mode Line changes
    ;; Display workgroups in Mode Line?
    (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
    (setq wg-flag-modified t)                 ; Display modified flags as well
    (setq wg-mode-line-decor-left-brace "["
          wg-mode-line-decor-right-brace "]"  ; how to surround it
          wg-mode-line-decor-divider ":")

    (workgroups-mode 1)
    (spacemacs|hide-lighter workgroups-mode)
    )
  ))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
