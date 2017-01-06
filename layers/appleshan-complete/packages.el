;;; packages.el --- appleshan-complete layer packages file for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;  这个文件是 appleshan 个人专用的 emacs 配置文件，emacs 中文用户可以参考。

;;; License: GPLv3

;;; Code:

(setq appleshan-complete-packages
    '(
      company
      helm-flx
      helm-fuzzier
      projectile
      yasnippet
      ))

;; List of packages to exclude.
(setq appleshan-complete-excluded-packages '())

;; company-mode
(defun appleshan-complete/post-init-company ()
  (progn
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.5)

    (setq company-selection-wrap-around t)
    (setq company-show-numbers t)

    ;; company-dabbrev
    (setq company-dabbrev-char-regexp "[[:word:]_:@.-]+")
    (setq company-dabbrev-minimum-length 2)

    (spacemacs|add-company-backends :modes
      text-mode
      shell-script-mode
      sh-mode
      nxml-mode
      conf-unix-mode
      json-mode
      graphviz-dot-mode)
  ))

(defun appleshan-complete/post-init-helm-flx ()
  ;; garbage collection
  (defun eos/minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun eos/minibuffer-exit-hook ()
    ;; 20mb
    (setq gc-cons-threshold (* 20 1024 1024)))

  (add-hook 'minibuffer-setup-hook #'eos/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'eos/minibuffer-exit-hook))

(defun appleshan-complete/init-helm-fuzzier ()
  (use-package helm-fuzzier
    :ensure t
    :init
    (helm-fuzzier-mode)))

(defun appleshan-complete/post-init-projectile ()
  (with-eval-after-load 'projectile
    ;; global ignores
    (add-to-list 'projectile-globally-ignored-files ".tern-port")
    (add-to-list 'projectile-globally-ignored-files "GTAGS")
    (add-to-list 'projectile-globally-ignored-files "GPATH")
    (add-to-list 'projectile-globally-ignored-files "GRTAGS")
    (add-to-list 'projectile-globally-ignored-files "GSYMS")
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    ;; always ignore .class files
    (add-to-list 'projectile-globally-ignored-file-suffixes ".class")))

(defun appleshan-complete/post-init-yasnippet ()
  (setq yas-snippet-dirs (delq 'yas-installed-snippets-dir yas-snippet-dirs))

  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

  (set-face-background 'secondary-selection "gray")

  (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
