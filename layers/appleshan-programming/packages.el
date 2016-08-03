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
      engine-mode
      flycheck
      flycheck-package
      gist
      highlight-escape-sequences
      magit
      ; origami
      paredit
      prodigy
      projectile
      ; puml-mode
      tldr
      yasnippet
      zeal-at-point
      ))

;; List of packages to exclude.
(setq appleshan-programming-excluded-packages '())

(defun appleshan-programming/post-init-engine-mode ()
  (with-eval-after-load 'engine-mode
    (add-to-list 'search-engine-alist
      ;; elisp code search
      (Elisp
           :name "Elisp code search"
           :url "http://www.google.com.au/search?q=%s+filetype:el")
      ;; javascript search on mozilla.org
      (Elisp
           :name "Javascript search on mozilla.org"
           :url "http://www.google.com.au/search?q=%s+site:developer.mozilla.org")
      )
    ))

(defun appleshan-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (setq flycheck-display-errors-delay 0.2)))

(defun appleshan-programming/init-flycheck-package ()
  (use-package flycheck-package))

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

(defun appleshan-programming/init-highlight-escape-sequences ()
  (use-package highlight-escape-sequences
    :defer t
    :init
    (hes-mode)))

(defun appleshan-programming/post-init-magit ()
  (with-eval-after-load 'magit
    (add-to-list 'magit-no-confirm 'stage-all-changes)
    (define-key magit-log-mode-map (kbd "W") 'magit-copy-section-value)
    (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
    (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
    (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
    (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)
    ; (setq magit-completing-read-function 'magit-builtin-completing-read)
    (setq magit-completing-read-function 'ivy-completing-read)
    ))

; (defun appleshan-programming/init-origami ()
;   (use-package origami
;     :defer t
;     :init
;     (progn
;       (defun my-origami-hook ()
;         (origami-mode 1))
;       (add-to-list 'prog-mode-hook 'my-origami-hook))
;     :config
;     (progn
;       (define-key origami-mode-map (kbd "<C-tab>") 'origami-toggle-node)
;       ; (define-key term-raw-map (kbd "<C-S-tab>") 'origami-toggle-all-nodes)
;       )
;     ))

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

(defun appleshan-programming/post-init-projectile ()
  (use-package projectile
    :config
    (progn
      (setq projectile-completion-system 'ivy)
    )))

;; TODO: slow!!!!
; (defun appleshan-programming/init-puml-mode ()
;   (use-package puml-mode
;     :defer t
;     :init
;     (progn
;       (setq puml-plantuml-jar-path
;         (concat user-home-directory "bin/develop/java/plantuml.jar"))
;       (puml-mode))
;     :config
;     (progn
;       ;; Enable puml-mode for PlantUML files
;       (add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
;       (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
;     )))

(defun appleshan-programming/init-tldr ()
  (use-package tldr
    :defer t
    :init
    (setq tldr-directory-path
      (concat user-home-directory ".config/tldr/"))))

(defun appleshan-programming/post-init-yasnippet ()
  (with-eval-after-load 'yasnippet
    (set-face-background 'secondary-selection "gray")
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))
    (defun appleshan/load-yasnippet ()
      (interactive)
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/local/snippets"))
          (setq yas-snippet-dirs  my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (spacemacs/add-to-hooks 'appleshan/load-yasnippet '(prog-mode-hook
                                                        markdown-mode-hook
                                                        org-mode-hook))
    ))

;; 使用 zeal 查看 docset
(defun appleshan-programming/post-init-zeal-at-point ()
  (with-eval-after-load 'zeal-at-point
    (spacemacs/declare-prefix "d" "dir/dash/zeal")
    (add-hook 'python-mode-hook
      (lambda () (setq zeal-at-point-docset "python 2")))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
