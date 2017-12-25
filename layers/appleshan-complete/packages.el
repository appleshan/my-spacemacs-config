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
      helm-ag
      helm-flx
      projectile
      yasnippet
      smart-tab
      ))

;; List of packages to exclude.
(setq appleshan-complete-excluded-packages '())

;; company-mode
(defun appleshan-complete/post-init-company ()
  (setq company-selection-wrap-around t
        ;; do or don't automatically start completion after <idle time>
        company-idle-delay 0.5
        ;; at least 1 letters need to be there though
        company-minimum-prefix-length 1
        ;; show completion numbers for hotkeys
        company-show-numbers t
        ;; align annotations to the right
        company-tooltip-align-annotations t
        company-search-regexp-function #'company-search-flex-regexp)

  (spacemacs|add-company-backends :modes
    ; !!! NOTE: 不能为 text-mode 开启 company , 当在 magit 中填写 commit message 的时候，会造成 emacs 崩溃. !!!
    ; text-mode
    shell-script-mode
    sh-mode
    nxml-mode
    conf-unix-mode
    json-mode
    graphviz-dot-mode)
  )

(defun appleshan-complete/post-init-helm-ag ()
  (setq helm-ag-base-command "sift --no-color -n")
  ; (setq helm-ag-command-option "--all-text")
  (setq helm-ag-insert-at-point 'symbol)
  (setq helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'")))

(defun appleshan-complete/post-init-helm-flx ()
  ;; garbage collection
  (defun eos/minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun eos/minibuffer-exit-hook ()
    ;; 20mb
    (setq gc-cons-threshold (* 20 1024 1024)))

  (add-hook 'minibuffer-setup-hook #'eos/minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'eos/minibuffer-exit-hook))

(defun appleshan-complete/post-init-projectile ()
  (with-eval-after-load 'projectile
    ;; global ignores
    (add-to-list 'projectile-globally-ignored-files ".tern-port")
    (add-to-list 'projectile-globally-ignored-files "GTAGS")
    (add-to-list 'projectile-globally-ignored-files "GPATH")
    (add-to-list 'projectile-globally-ignored-files "GRTAGS")
    (add-to-list 'projectile-globally-ignored-files "GSYMS")
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    ;; always ignore .class / .pyc files
    (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
    (add-to-list 'projectile-globally-ignored-file-suffixes ".pyc")))

(defun appleshan-complete/post-init-yasnippet ()
  (setq yas-snippet-dirs (delq 'yas-installed-snippets-dir yas-snippet-dirs))
  ;; ~/.emacs.d/elpa/yasnippet-xxxxx/snippets
  ;; 不再加载这个目录，拷贝需要的到 ~/.spacemacs.d/snippets
  ;; (push yas-installed-snippets-dir yas-snippet-dirs)

  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

  (set-face-background 'secondary-selection "gray")

  (use-package dropdown-list :commands dropdown-list)
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-x-prompt
                               yas-maybe-ido-prompt
                               yas-completing-prompt)))

;; Used smart-tab to complete everywhere except for ERC, shell and mu4e.
(defun appleshan-complete/init-smart-tab ()
  (use-package smart-tab
    :ensure t
    :defer t
    :diminish ""
    :init
    (global-smart-tab-mode 1)
    (setq smart-tab-using-hippie-expand t)
    :config
    (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'shell-mode)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
