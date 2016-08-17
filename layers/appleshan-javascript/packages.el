;;; packages.el --- appleshan-javascript Layer packages File for Spacemacs
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
(setq appleshan-javascript-packages
    '(
      ; amd-mode
      css-mode
      impatient-mode
      json-mode
      js-doc
      js2-mode
      js2-refactor
      tern
      ; web-mode
      ; xref-js2
      ))

;; List of packages to exclude.
(setq appleshan-javascript-excluded-packages '())

;; TODO: startup slow!!!!
;; 启动 amd-mode 会造成 emacs 首页的 Projects 列表不能显示
; (defun appleshan-javascript/init-amd-mode ()
;   (use-package amd-mode
;     :defer t
;     :init
;     (progn
;       (require 'amd-mode)
;       (amd-mode 1))
;     :config
;     (progn
;       (define-key amd-mode-map (kbd "C-c C-a") #'amd-initialize-makey-group)
;       )))

(defun appleshan-javascript/post-init-css-mode ()
  (with-eval-after-load 'css-mode
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))))

(defun appleshan-javascript/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn

      (defun my-impatient-mode-hook ()
        "my web mode hook for HTML REPL"
        (interactive)
        (impatient-mode)
        (spacemacs|hide-lighter impatient-mode)
        (httpd-start))

      (add-hook 'web-mode-hook 'my-impatient-mode-hook)
      (spacemacs/set-leader-keys-for-major-mode 'web-mode
        "p" 'imp-visit-buffer)
      )))

(defun appleshan-javascript/post-init-json-mode ()
  (with-eval-after-load 'json-mode
    (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))))

(defun appleshan-javascript/post-init-js-doc ()
  (with-eval-after-load 'js-doc
    (setq js-doc-mail-address "apple.shan@gmail.com"
          js-doc-author (format "Apple Shan <%s>" js-doc-mail-address)
          js-doc-url "https://github.com/appleshan/"
          js-doc-license "MIT")

    (defun my-js-doc-insert-function-doc-snippet ()
      "Insert JsDoc style comment of the function with yasnippet."
      (interactive)

      (with-eval-after-load 'yasnippet
        (js-doc--beginning-of-defun)

        (let ((metadata (js-doc--function-doc-metadata))
              (field-count 1))
          (yas-expand-snippet
           (concat
            js-doc-top-line
            " * ${1:Function description.}\n"
            (format "* @method %s\n" (nth-value 1 (split-string (which-function) "\\.")))
            (mapconcat (lambda (param)
                         (format
                          " * @param {${%d:Type of %s}} %s - ${%d:Parameter description.}\n"
                          (incf field-count)
                          param
                          param
                          (incf field-count)))
                       (cdr (assoc 'params metadata))
                       "")
            (when (assoc 'returns metadata)
              (format
               " * @returns {${%d:Return Type}} ${%d:Return description.}\n"
               (incf field-count)
               (incf field-count)))
            (when (assoc 'throws metadata)
              (format
               " * @throws {${%d:Exception Type}} ${%d:Exception description.}\n"
               (incf field-count)
               (incf field-count)))
            js-doc-bottom-line)))))

    (add-hook 'js2-mode-hook
            #'(lambda ()
                (define-key js2-mode-map "\C-ci" 'my-js-doc-insert-function-doc-snippet)
                (define-key js2-mode-map "@" 'js-doc-insert-tag)))))

(defun appleshan-javascript/post-init-js2-mode ()
  (progn
    (setq company-backends-js2-mode '((company-dabbrev-code :with company-keywords company-etags)
                                      company-files
                                      company-dabbrev))

    (add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "tb" 'appleshan/company-toggle-company-tern)

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)

    (spacemacs/set-leader-keys-for-major-mode 'web-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)

    (defun my-js2-mode-hook ()
      (progn
        (setq mode-name "JS2")
        (define-key js2-mode-map   (kbd "s-.") 'company-tern)

        (setq forward-sexp-function nil)
        ;; (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
        (set (make-local-variable 'semantic-mode) nil)
        ))
    (add-hook 'js2-mode-hook 'my-js2-mode-hook)

    ;; http://blog.binchen.org/posts/use-which-func-mode-with-js2-mode.html
    (defun my-which-function ()
      ;; clean the imenu cache
      ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
      (setq imenu--index-alist nil)
      (which-function))

    (add-hook 'js2-mode-hook 'which-function-mode)

    (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")


    (with-eval-after-load 'js2-mode
      (progn
        ;; these mode related variables must be in eval-after-load
        ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
        (setq-default js2-allow-rhino-new-expr-initializer nil)
        (setq-default js2-auto-indent-p nil)
        (setq-default js2-enter-indents-newline nil)
        (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq-default js2-idle-timer-delay 0.2)
        (setq-default js2-mirror-mode nil)
        (setq-default js2-strict-inconsistent-return-warning nil)
        (setq-default js2-include-rhino-externs nil)
        (setq-default js2-include-gears-externs nil)
        (setq-default js2-concat-multiline-strings 'eol)
        (setq-default js2-rebind-eol-bol-keys nil)
        (setq-default js2-auto-indent-p t)

        (setq-default js2-bounce-indent nil)
        (setq-default js-indent-level 4)
        (setq-default js2-basic-offset 4)
        (setq-default js2-indent-switch-body t)
        ;; Let flycheck handle parse errors
        ;; (setq-default js2-mode-show-parse-errors nil)
        ;; (setq-default js2-mode-show-strict-warnings nil)
        (setq-default js2-highlight-external-variables t)
        (setq-default js2-strict-trailing-comma-warning nil)

        (defun my-web-mode-indent-setup ()
          (setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
          (setq web-mode-css-indent-offset 2)    ; web-mode, css in html file
          (setq web-mode-code-indent-offset 2) ; web-mode, js code in html file
          )

        (add-hook 'web-mode-hook 'my-web-mode-indent-setup)

        (defun my-toggle-web-indent ()
          (interactive)
          ;; web development
          (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
              (progn
                (setq js-indent-level (if (= js-indent-level 2) 4 2))
                (setq js2-basic-offset (if (= js2-basic-offset 2) 4 2))))

          (if (eq major-mode 'web-mode)
              (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
                     (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
                     (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
          (if (eq major-mode 'css-mode)
              (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

          (setq indent-tabs-mode nil))


        (spacemacs/set-leader-keys-for-major-mode 'js2-mode
          "oi" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'js-mode
          "oi" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'web-mode
          "oi" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'css-mode
          "oi" 'my-toggle-web-indent)

        (spacemacs/declare-prefix-for-mode 'js2-mode "mo" "toggle")
        (spacemacs/declare-prefix-for-mode 'js-mode "mo" "toggle")
        (spacemacs/declare-prefix-for-mode 'web-mode "mo" "toggle")
        (spacemacs/declare-prefix-for-mode 'css-mode "mo" "toggle")

        (autoload 'flycheck-get-checker-for-buffer "flycheck")
        (defun sanityinc/disable-js2-checks-if-flycheck-active ()
          (unless (flycheck-get-checker-for-buffer)
            (set (make-local-variable 'js2-mode-show-parse-errors) t)
            (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
        (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
        
        (eval-after-load 'tern-mode
          '(spacemacs|hide-lighter tern-mode))
        ))

    (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map)
    ))

(defun appleshan-javascript/post-init-js2-refactor ()
  (with-eval-after-load 'js2-refactor
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "r>" 'js2r-forward-slurp
      "r<" 'js2r-forward-barf)))

(defun appleshan-javascript/post-init-tern ()
  (with-eval-after-load 'tern
    ;; tern will override js2r keybindings...
    (define-key tern-mode-keymap (kbd "C-c C-r") nil)

    ;; ... and xref.
    (define-key tern-mode-keymap (kbd "M-.") nil)
    (define-key tern-mode-keymap (kbd "M-,") nil)
    ))

; (defun appleshan-programming/post-init-web-mode ()
;     (push '((company-dabbrev-code
;              company-keywords
;              company-etags)
;             company-files
;             company-dabbrev)
;           company-backends-web-mode))

; (defun appleshan-javascript/init-xref-js2 ()
;   (use-package xref-js2
;     :defer t
;     :init
;     (progn
;       ;; add xref-js2 support
;       (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
;     :config
;     (progn
;       ;; We have JS files in Scripts directories, ignore that
;       (add-to-list 'xref-js2-ignored-dirs "Scripts")
;       ;; Also ignore some other files
;       ; (dolist (file '("require.js"
;       ;                 "highcharts.js"
;       ;                 "highcharts.src.js"
;       ;                 "bootstrap.js"
;       ;                 "Gruntfile.js"
;       ;                 "moment.js"
;       ;                 "moment-with-locales.js"))
;       ;   (add-to-list 'xref-js2-ignored-files file))
;       )))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
