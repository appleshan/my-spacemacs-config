;;; funcs.el --- appleshan-lisp Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'smartparens
  :config
  ;;; Java
  (sp-with-modes '(java-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))
)

;; {{{ intellij-java-style
;; via http://emacs.stackexchange.com/questions/17327/how-to-have-c-offset-style-correctly-detect-a-java-constructor-and-change-indent
(defun my/point-in-defun-declaration-p ()
  (let ((bod (save-excursion (c-beginning-of-defun)
                             (point))))
    (<= bod
        (point)
        (save-excursion (goto-char bod)
                        (re-search-forward "{")
                        (point)))))

(defun my/is-string-concatenation-p ()
  "Returns true if the previous line is a string concatenation"
  (save-excursion
    (let ((start (point)))
      (forward-line -1)
      (if (re-search-forward " \\\+$" start t) t nil))))

(defun my/inside-java-lambda-p ()
  "Returns true if point is the first statement inside of a lambda"
  (save-excursion
    (c-beginning-of-statement-1)
    (let ((start (point)))
      (forward-line -1)
      (if (search-forward " -> {" start t) t nil))))

(defun my/trailing-paren-p ()
  "Returns true if point is a training paren and semicolon"
  (save-excursion
    (end-of-line)
    (let ((endpoint (point)))
      (beginning-of-line)
      (if (re-search-forward "[ ]*);$" endpoint t) t nil))))

(defun my/prev-line-call-with-no-args-p ()
  "Return true if the previous line is a function call with no arguments"
  (save-excursion
    (let ((start (point)))
      (forward-line -1)
      (if (re-search-forward ".($" start t) t nil))))

(defun my/arglist-cont-nonempty-indentation (arg)
  (if (my/inside-java-lambda-p)
      '+
    (if (my/is-string-concatenation-p)
        16 ;; TODO don't hard-code
      (unless (my/point-in-defun-declaration-p) '++))))

(defun my/statement-block-intro (arg)
  (if (and (c-at-statement-start-p) (my/inside-java-lambda-p)) 0 '+))

(defun my/block-close (arg)
  (if (my/inside-java-lambda-p) '- 0))

(defun my/arglist-close (arg) (if (my/trailing-paren-p) 0 '--))

(defun my/arglist-intro (arg)
  (if (my/prev-line-call-with-no-args-p) '++ 0))

(defconst intellij-java-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    ;; the following preserves Javadoc starter lines
    (c-offsets-alist
     .
     ((inline-open . 0)
      (topmost-intro-cont    . +)
      (statement-block-intro . my/statement-block-intro)
      (block-close           . my/block-close)
      (knr-argdecl-intro     . +)
      (substatement-open     . +)
      (substatement-label    . +)
      (case-label            . +)
      (label                 . +)
      (statement-case-open   . +)
      (statement-cont        . +)
      (arglist-intro         . my/arglist-intro)
      (arglist-cont-nonempty . (my/arglist-cont-nonempty-indentation c-lineup-arglist))
      (arglist-close         . my/arglist-close)
      (inexpr-class          . 0)
      (access-label          . 0)
      (inher-intro           . ++)
      (inher-cont            . ++)
      (brace-list-intro      . +)
      (func-decl-cont        . ++))))
  "Intellij Java Programming Style")

(c-add-style "intellij" intellij-java-style)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(customize-set-variable 'c-default-style
                        '((java-mode . "intellij")
                          (awk-mode . "awk")
                          (other . "gnu")))
;; }}}

(defun eos/setup-java ()
  (interactive)
  (define-key java-mode-map (kbd "M-,") 'pop-tag-mark)
  (define-key java-mode-map (kbd "C-c M-i") 'java-imports-add-import-dwim)
  (c-set-style "intellij" t)
  (subword-mode 1)
  (toggle-truncate-lines 1)
  ;; Generic java stuff things
  (setq-local fci-rule-column 99)
  (when (fboundp 'eos/turn-on-whitespace-mode)
    (whitespace-mode -1)
    (eos/turn-on-whitespace-mode))
  ;; remove the stupid company-eclim backend
  (when (boundp 'company-backends)
    (setq company-backends (delete 'company-eclim company-backends)))
  ;; hide the initial comment in the file (usually a license) if hs-minor-mode
  ;; is enabled
  (when (boundp' hs-minor-mode)
    (hs-hide-initial-comment-block)))

(add-hook 'java-mode-hook #'eos/setup-java)

;; Make emacs' compile recognize broken gradle output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("^:[^/.\n]+\\(/.+\\):\\([[:digit:]]+\\):" 1 2))

;; {{ maven
;; mvn compile
(defun mvn-compile-full ()
  (interactive)
  (mvn "dependency:sources"))

;; mvn package
(defun mvn-package ()
  (interactive)
  (mvn "package"))

;; mvn install
(defun mvn-install ()
  (interactive)
  (mvn "install"))
;; }}

(defun eclim/file-locate-ivy ()
  (interactive)
  (eclim/with-results hits ("locate_file" ("-p" "^.*$") ("-s" "workspace"))
    (find-file
      (ivy-completing-read "Select file:"
        (mapcar (lambda (hit) (assoc-default 'path hit)) hits)))))

;; {{ show documentation
(defun eclim//show-postip (doc)
  ;; TODO(jaigupta): Check if we can make use of the links in docs.
  (pos-tip-show (assoc-default 'text doc)))

;; Show a small java doc at point without opening a new buffer.
(defun eclim/java-show-documentation-for-current-element-postip ()
  (interactive)
    (let ((symbol (symbol-at-point)))
      (if symbol
          (let ((bounds (bounds-of-thing-at-point 'symbol))
                (window-config (current-window-configuration)))
            (eclim/with-results doc ("java_element_doc"
                                     ("-p" (eclim--project-name))
                                     "-f"
                                     ("-l" (- (cdr bounds) (car bounds)))
                                     ("-o" (save-excursion
                                             (goto-char (car bounds))
                                             (eclim--byte-offset))))
              (eclim//show-postip doc)))

        (message "No element found at point."))))
;; }}

; java run Main()
(defun java-run-standalone ()
  "Running small standalone programs."
  (interactive)
  (save-some-buffers 1)
  (compile (concat "javac_java.sh " (buffer-file-name (current-buffer)))))

(defun get-trace (segfault)
  "Following exceptions and spontaneous backtraces paths in Emacs"
  (interactive)
  (compile (concat "show-trace-in-emacs.pl" (if segfault " --segfault"))))

(defmacro ilambda (&rest body) `(lambda () (interactive) ,@body))

(add-hook 'java-mode-hook
  '(lambda ()
     (local-set-key [(control return)] (ilambda (javacomp-standalone)))
     (local-set-key "\M-=" (ilambda (get-trace nil)))
     (local-set-key "\M--" (ilambda (get-trace t)))
         ))

(setq completion-ignored-extensions (cons ".class" completion-ignored-extensions))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
