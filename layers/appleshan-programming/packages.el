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
      (lsp-mode :location (recipe
                           :fetcher github
                           :repo "emacs-lsp/lsp-mode"))
      (lsp-ui :location (recipe
                         :fetcher github
                         :repo "emacs-lsp/lsp-ui"))
      (company-lsp :location (recipe
                              :fetcher github
                              :repo "tigersoldier/company-lsp"))
      ;; helm-xref
      engine-mode
      flycheck
      flycheck-inline
      flycheck-package
      flycheck-pycheckers
      avy-flycheck
      gist
      git-messenger
      highlight-escape-sequences
      highlight-indent-guides
      highlight-thing
      indent-tools
      magit
      paredit
      prodigy
      plantuml-mode
      quickrun
      symbol-overlay
      tldr
      vdiff
      wakatime-mode
      zeal-at-point
      ))

;; List of packages to exclude.
(setq appleshan-programming-excluded-packages '())

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(defun appleshan-programming/init-lsp-mode ()
  (use-package lsp-mode
    :diminish lsp-mode
    :config
    (with-eval-after-load 'flycheck
      (require 'lsp-flycheck))
    ))

(defun appleshan-programming/init-lsp-ui ()
  (use-package lsp-ui
    :after lsp-mode
    :config
    (setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame

    (setq lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))
    (advice-add 'lsp-ui-peek--goto-xref :around #'my-advice/xref-set-jump)

    (evil-make-overriding-map lsp-ui-peek-mode-map 'normal)
    (define-key lsp-ui-peek-mode-map (kbd "h") 'lsp-ui-peek--select-prev-file)
    (define-key lsp-ui-peek-mode-map (kbd "l") 'lsp-ui-peek--select-next-file)
    (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
    (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)

    (define-key lsp-ui-mode-map (kbd "C-c l") 'lsp-ui-imenu)

    (setq lsp-ui-sideline-show-flycheck nil)
    (setq lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
    (setq lsp-ui-sideline-ignore-duplicate t)
    (set-face-attribute 'lsp-ui-sideline-symbol nil :foreground "grey30" :box nil)
    (set-face-attribute 'lsp-ui-sideline-current-symbol nil :foreground "grey38" :box nil)
    (when (internal-lisp-face-p 'lsp-ui-sideline-contents)
      (set-face-attribute 'lsp-ui-sideline-contents nil :foreground "grey35")
      (set-face-attribute 'lsp-ui-sideline-current-contents nil :foreground "grey43"))

    (with-eval-after-load 'lsp-mode
      (add-hook 'lsp-after-open-hook
        (lambda ()
          (lsp-ui-mode 1)
          ;; bug: flycheck-display-errors-function 被 lsp-ui-sideline 覆盖
          ;; 然后重新设置为 flycheck-inline-error-messages
          (setq-local flycheck-display-errors-function 'flycheck-inline-error-messages)
          )))
    ))

;; `company' backend for `lsp-mode'
(defun appleshan-programming/init-company-lsp ()
  (use-package company-lsp
    :config
    (with-eval-after-load 'company
      :init (push '(company-lsp :with company-yasnippet) company-backends))
    ))

(defun appleshan-programming/init-helm-xref ()
  (use-package helm-xref
    :config
    ;; This is required to make xref-find-references work in helm-mode.
    ;; In helm-mode, it gives a prompt and asks the identifier (which has no text property)
    ;; and then passes it to lsp-mode, which requires the text property at point to locate the references.
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
    (setq xref-prompt-for-identifier
      '(not xref-find-definitions
            xref-find-definitions-other-window
            xref-find-definitions-other-frame
            xref-find-references
            spacemacs/jump-to-definition
            spacemacs/jump-to-reference
            ))
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)))

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

(defun appleshan-programming/pre-init-flycheck ()
  (spacemacs|use-package-add-hook flycheck
    :post-init
    (progn
      ;; (setq flycheck-indication-mode 'right-fringe)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      )))

(defun appleshan-programming/post-init-flycheck ()
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc json-jsonlint json-python-json))
  (setq flycheck-display-errors-delay 0.9)
  (setq flycheck-idle-change-delay 2.0)

  (flycheck-define-checker xml-xmllint
    "A XML syntax checker and validator using the xmllint utility.

  The xmllint is part of libxml2, see URL
  `http://www.xmlsoft.org/'."
    :command ("xmllint" "--noout" source)
    :error-patterns
    ((error line-start (file-name) ":" line ": " (message) line-end))
    :modes (xml-mode nxml-mode)))

(defun appleshan-programming/init-flycheck-inline ()
  (use-package flycheck-inline
    :ensure t
    :defer t
    :init
    ;; 这里设置的 flycheck-display-errors-function 会被 lsp-ui 覆盖
    ;; (add-hook 'flycheck-mode-hook 'flycheck-inline-enable)
    ))

(defun appleshan-programming/init-flycheck-package ()
  (use-package flycheck-package
    :init
    (eval-after-load 'flycheck '(flycheck-package-setup))))

(defun appleshan-programming/init-flycheck-pycheckers ()
  (use-package flycheck-pycheckers
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))))

;; Jump to and fix syntax errors via `avy'
(defun appleshan-programming/init-avy-flycheck ()
  (use-package avy-flycheck
    :init (avy-flycheck-setup)))

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

;;; Pop up last commit information of current line
(defun appleshan-programming/post-init-git-messenger ()
  (with-eval-after-load 'git-messenger
    (define-key git-messenger-map (kbd "f") 'appleshan/github-browse-commit)))

(defun appleshan-programming/init-highlight-escape-sequences ()
  (use-package highlight-escape-sequences
    :defer t
    :init
    (hes-mode)))

(defun appleshan-programming/init-highlight-indent-guides ()
  (use-package highlight-indent-guides
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
      (spacemacs|diminish highlight-indent-guides-mode))
    ; :config (setq highlight-indent-guides-method 'character) ; 'fill / 'column / 'character
    ))

(defun appleshan-programming/init-highlight-thing ()
  (use-package highlight-thing
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-thing-mode)
      (spacemacs|diminish highlight-thing-mode))
    :config
    (progn
      (setq highlight-thing-delay-seconds 1.5)
      (setq highlight-thing-case-sensitive-p t))))

(defun appleshan-programming/init-indent-tools ()
  (use-package indent-tools
    :config
    (global-set-key (kbd "C-c i") 'indent-tools-hydra/body)))

(defun appleshan-programming/post-init-magit ()
  (with-eval-after-load 'magit
    (if (file-exists-p  "/usr/bin/emacsclient")
        (setq magit-emacsclient-executable "/usr/bin/emacsclient")
      (setq magit-emacsclient-executable (executable-find "emacsclient")))

    (add-to-list 'magit-no-confirm 'stage-all-changes)
    (define-key magit-mode-map (kbd "C-c C-b") 'appleshan/magit-browse)
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
    (setq plantuml-jar-path (concat (getenv "JAR_PATH") "/plantuml.jar"))
    ))

(defun appleshan-programming/post-init-plantuml-mode ()
  ;; Enable puml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))

;;; Compiling, Running Code
(defun appleshan-programming/init-quickrun ()
  (use-package quickrun))

;; Highlight symbols
(defun appleshan-programming/init-symbol-overlay ()
  (use-package symbol-overlay
    :diminish symbol-overlay-mode
    :init (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  ))

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

(defun appleshan-programming/post-init-wakatime-mode ()
  (spacemacs|diminish wakatime-mode))

;; 使用 zeal 查看 docset
(defun appleshan-programming/post-init-zeal-at-point ()
  (add-hook 'python-mode-hook
    (lambda () (setq zeal-at-point-docset "python3"))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
