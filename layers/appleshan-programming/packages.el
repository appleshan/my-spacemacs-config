;;; packages.el --- appleshan-programming Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq appleshan-programming-packages
    '(
      editorconfig
      engine-mode
      flycheck
      flycheck-package
      gist
      git-messenger
      highlight-escape-sequences
      highlight-thing
      magit
      paredit
      paren-face
      prodigy
      plantuml-mode
      smart-tab
      tldr
      vdiff
      zeal-at-point
      ))

;; List of packages to exclude.
(setq appleshan-programming-excluded-packages '())

(defun appleshan-programming/init-editorconfig ()
  (use-package editorconfig
    :init
    (progn
      (defun conditional-enable-editorconfig ()
        (if (and (appleshan/vcs-project-root)
                 (locate-dominating-file default-directory ".editorconfig"))
            (editorconfig-apply)))
      (add-hook 'prog-mode-hook 'conditional-enable-editorconfig))))

(defun appleshan-programming/post-init-engine-mode ()
  (add-to-list 'search-engine-alist
    ;; elisp code search
    '(Elisp
         :name "Elisp code search"
         :url "http://www.google.com.au/search?q=%s+filetype:el")
    ;; javascript search on mozilla.org
    '(Elisp
         :name "Javascript search on mozilla.org"
         :url "http://www.google.com.au/search?q=%s+site:developer.mozilla.org")
    ))

;; (defun appleshan-programming/pre-init-flycheck ()
;;   (spacemacs|use-package-add-hook flycheck
;;     :post-init
;;     (progn
;;       ;; (setq flycheck-indication-mode 'right-fringe)
;;       ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;       )))

(defun appleshan-programming/post-init-flycheck ()
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc json-jsonlint json-python-json))
  (setq flycheck-display-errors-delay 0.9)
  (setq flycheck-idle-change-delay 2.0))

(defun appleshan-programming/init-flycheck-package ()
  (use-package flycheck-package
    :init
    (eval-after-load 'flycheck '(flycheck-package-setup))))

(defun appleshan-programming/post-init-gist ()
  (use-package gist
    :defer t
    :init
    (setq gist-list-format
      '((files "File" 30 nil "%s")
        (id "Id" 10 nil identity)
        (created "Created" 20 nil "%D %R")
        (visibility "Visibility" 10 nil
                    (lambda
                      (public)
                      (or
                       (and public "public")
                       "private")))
        (description "Description" 0 nil identity)))
    :config
    (progn
      (spacemacs|define-transient-state gist-list-mode
        :title "Gist-mode Transient State"
        :bindings
        ("k" gist-kill-current "delete gist")
        ("e" gist-edit-current-description "edit gist title")
        ("+" gist-add-buffer "add a file")
        ("-" gist-remove-file "delete a file")
        ("y" gist-print-current-url "print url")
        ("b" gist-browse-current-url "browse gist in browser")
        ("*" gist-star "star gist")
        ("^" gist-unstar "unstar gist")
        ("f" gist-fork "fork gist")
        ("q" nil "quit" :exit t)
        ("<escape>" nil nil :exit t))
      (spacemacs/set-leader-keys-for-major-mode 'gist-list-mode
        "." 'spacemacs/gist-list-mode-transient-state/body))
    ))

(defun appleshan-programming/post-init-git-messenger ()
  (with-eval-after-load 'git-messenger
    (define-key git-messenger-map (kbd "f") 'appleshan/github-browse-commit)))

(defun appleshan-programming/init-highlight-escape-sequences ()
  (use-package highlight-escape-sequences
    :defer t
    :init
    (hes-mode)))

(defun appleshan-programming/init-highlight-thing ()
  (use-package highlight-thing
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-thing-mode)
      (spacemacs|hide-lighter highlight-thing-mode))
    :config
    (progn
      (setq highlight-thing-delay-seconds 1.5)
      (setq highlight-thing-case-sensitive-p t))))

(defun appleshan-programming/post-init-magit ()
  (with-eval-after-load 'magit
    (if (file-exists-p  "/usr/bin/emacsclient")
        (setq magit-emacsclient-executable "/usr/bin/emacsclient")
      (setq magit-emacsclient-executable (executable-find "emacsclient")))

    (add-to-list 'magit-no-confirm 'stage-all-changes)
    (define-key magit-mode-map "@" 'appleshan/magit-branch-pull-request)
    (define-key magit-log-mode-map (kbd "W") 'magit-copy-section-value)
    (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
    (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
    (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
    (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)

    (setq magit-process-popup-time 10
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)

    ;; http://emacs.stackexchange.com/questions/6021/change-a-branchs-upstream-with-magit/6023#6023
    (magit-define-popup-switch 'magit-push-popup ?u
      "Set upstream" "--set-upstream")
  ))

(defun appleshan-programming/init-paredit ()
  (use-package paredit
    :commands (paredit-wrap-round
               paredit-wrap-square
               paredit-wrap-curly
               paredit-splice-sexp-killing-backward)
    :init
    (progn
      (bind-key* "s-j"
                 #'paredit-splice-sexp-killing-backward)

      (bind-key* "s-(" #'paredit-wrap-round)
      (bind-key* "s-[" #'paredit-wrap-square)
      (bind-key* "s-{" #'paredit-wrap-curly)
      )))

(defun appleshan-programming/init-paren-face ()
  (use-package paren-face
    :ensure t
    :init (global-paren-face-mode)))

(defun appleshan-programming/post-init-prodigy ()
  (with-eval-after-load 'prodigy
    (defface prodigy-dull-face
      '((((class color)) :foreground "#999999"))
      "Gray color indicating waiting."
      :group 'prodigy)

    (setq prodigy-view-buffer-maximum-size 2048
          prodigy-view-truncate-by-default t)

    (prodigy-define-status :id 'running :face 'prodigy-dull-face)
    (prodigy-define-status :id 'exception :face 'prodigy-red-face)

    (setq prodigy-service-file (concat dotspacemacs-directory "local/prodigy-services.el"))
    (when (file-exists-p prodigy-service-file)
      (load-file prodigy-service-file))
  ))

(defun appleshan-programming/pre-init-plantuml-mode ()
  (spacemacs|use-package-add-hook plantuml-mode
    :post-config
    (progn
      (setq plantuml-jar-path "/opt/java-lib/plantuml.jar")
    )))

(defun appleshan-programming/post-init-plantuml-mode ()
  ;; Enable puml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

(defun appleshan-programming/init-smart-tab ()
  (use-package smart-tab
    :ensure t
    :defer t
    :diminish ""
    :init (global-smart-tab-mode 1)
    :config
    (setq smart-tab-using-hippie-expand t)
    (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'shell-mode)))

(defun appleshan-programming/init-tldr ()
  (use-package tldr
    :defer t
    :init
    (setq tldr-directory-path
      (concat user-home-directory ".config/tldr/"))))

(defun appleshan-programming/init-vdiff ()
  (use-package vdiff
    :defer t
    :commands (vdiff-buffers vdiff-files)
    :config
    (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)))

;; 使用 zeal 查看 docset
(defun appleshan-programming/post-init-zeal-at-point ()
  (add-hook 'python-mode-hook
    (lambda () (setq zeal-at-point-docset "python 2"))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
