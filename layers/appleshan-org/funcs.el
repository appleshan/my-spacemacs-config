;;; funcs.el --- appleshan-org Layer functions File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;{{ 更好看的符号列表标记
;; @see https://github.com/lujun9972/emacs-document/blob/master/org-mode/%E5%B0%86org%E7%9C%8B%E6%88%90%E6%96%87%E5%AD%97%E5%A4%84%E7%90%86%E5%99%A8.org
;; 这段代码将所有行中匹配指定正则表达式的内容都显示为一个Unicode的圆形符号,
;; 该段正则的意思是“以 1 个或多个破折号开头,紧接着是一个空格”.
;; 用星号和破折号来作为符号列表的标记挺好的, 但是使用一个真正的圆形符号来作标示也不错:
(font-lock-add-keywords
 'org-mode
 '(("^\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;;}}

;; src_
(font-lock-add-keywords
 'org-mode
 '(("\\(src_\\)\\([^[{]+\\)\\(\\[:.*\\]\\)\\({\\)\\([^}]*\\)\\(}\\)"
    (1 '(:foreground "black" :weight 'normal :height 0.1)) ; src_ part
    (2 '(:foreground "cyan" :weight 'bold :height 0.8 :box '(:color "cyan"))) ; "lang" part.
    (3 '(:foreground "#555555" :height 0.7)) ; [:header arguments] part.
    (4 '(:foreground "#333333")) ; {
    (5 'org-code) ; "code..." part.
    (6 '(:foreground "#333333")) ; }
    )))

;; src without arguments
(font-lock-add-keywords
 'org-mode
 '(("\\(src_\\)\\([^[{]+\\)\\({\\)\\([^}]*\\)\\(}\\)"
    (1 '(:foreground "black" :weight 'normal :height 0.1)) ; src_ part
    (2 '(:foreground "cyan" :weight 'bold :height 0.8 :box '(:color "cyan"))) ; "lang" part.
    (3 '(:foreground "#333333")) ; {
    (4 'org-code) ; "code..." part.
    (5 '(:foreground "#333333")) ; }
    )))

;; inline babel call
;; ... call_<name>[<inside header arguments>](<arguments>)[<end header arguments>] ...
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\[\\(.*\\)\\](\\(.*\\))\\[\\(.*\\)\\]"
    ;; "\\(call_\\)\\([^[(]*\\)\\([([][^)]*]\\)+"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic :underline "dark gray")) ; <name>
    (3 '(:foreground "gray" :height 0.6)) ; [<inside header arguments>]
    (4 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    (5 '(:foreground "gray" :height 0.6)) ; [<end header arguments>]
    )))

;; call_<name>[<inside header arguments>](<arguments>)
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\[\\(.*\\)\\](\\(.*\\))"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic :underline "dark gray")) ; <name>
    (3 '(:foreground "gray" :height 0.6)) ; [<inside header argument>]
    (4 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    )))


;; call_<name>(arguments)
(font-lock-add-keywords
 'org-mode
 '(("\\(call_\\)\\([^[(]*\\)\\((.*)\\)"
    (1 '(:foreground "orange red" :height 0.6)) ; call_
    (2 '(:foreground "yellow" :weight 'bold :slant 'italic :underline "dark gray")) ; <name>
    (3 '(:foreground "cyan" :weight 'bold)) ; (<arguments>)
    )))

;; @@html:<kbd>...</kbd>@@, <kbd> </kbd>
(defface org-html-kbd-tag
  '((nil (:foreground "cyan" :background "#004A5D"
                      :box '(:color "cyan" :line-width 1)
                      ;; :weight 'bold
                      )))
  "Face for highlight Org-mode html tag @<kbd>...@</kbd> or @@html:<kbd>...</kbd>@@."
  :group 'org-faces)

;; @@html:<kbd>C-h h</kbd>@@
(font-lock-add-keywords
 'org-mode
 '(("@@html:<kbd>\\([^<]*\\)</kbd>@@"
    (1 'org-html-kbd-tag))))

;; @<kbd>C-h h@</kbd>
(font-lock-add-keywords
 'org-mode
 '(("@<kbd>\\([^@]*\\)@</kbd>"
    (1 'org-html-kbd-tag))))


(defun my/org-insert-key ()
  "Insert keybinding code in Org with a keybinding quickly.
In common insert mode or in select region text to press this keybinding \\<C-c k>.
to insert <kbd>..</kbd> (HTML) org =[..]= (Org-mode)."
  (interactive)
  (if (region-active-p)
      (let ((where (cons (region-beginning) (region-end))))
        (insert-pair where "=[" "]="))
    ;; (insert-pair nil "=[" "]=")
    (progn
      (insert "=[]=")
      (backward-char 2)))
  )

(defun my/org-insert-kbd ()
  "Insert literal HTML tag <kbd></kbd>."
  (interactive)
  (if (region-active-p)
      (let ((where (cons (region-beginning) (region-end))))
        (insert-pair where "@@html:<kbd>" "</kbd>@@"))
    (progn
      (insert "@@html:<kbd></kbd>@@ ")
      (backward-char 9)))
  )


;; (defun my/org-insert-kbd (key)
;;   "Ask for a KEY then insert its description.
;; Will work on both `org-mode' and any mode that accepts plain html."
;;   (interactive "kType key sequence: ")
;;   (let* ((is-org-mode (derived-mode-p 'org-mode))
;;          (tag (if is-org-mode
;;                   "@@html:<kbd>@@%s@@html:</kbd>@@"
;;                 "<kbd>%s</kbd>")))
;;     (if (null (equal key "
;; "))
;;         (insert
;;          (format tag (help-key-description key nil)))
;;       (insert (format tag ""))
;;       (forward-char (if is-org-mode -15 -6)))))

;;; Inserting the kbd tag in interactively
(eval-after-load 'ox-html
  ;; If you prefer to use ~ for <code> tags. Replace "code" with
  ;; "verbatim" here, and replace "~" with "=" below.
  '(push '(code . "<kbd>%s</kbd>") org-html-text-markup-alist))

(defun my/insert-key (key)
  "Ask for a KEY then insert its description.
Will work on both `org-mode' and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((orgp (derived-mode-p 'org-mode))
         (tag (if orgp
                  ;; "~%s~"
                  "=[%s]="
                ;; "@@html:<kbd>%s</kbd>@@"
                "<kbd>%s</kbd>")))
    (if (null (equal key "\C-m"))
        (insert
         (format tag (help-key-description key nil)))
      ;; If you just hit RET.
      (insert (format tag ""))
      (forward-char (if orgp -2 -6)))))

; (define-key org-mode-map (kbd "C-c K") 'my/insert-kbd)
; (define-key org-mode-map (kbd "C-c k") 'my/org-insert-key)


(with-eval-after-load 'smartparens
  :config
  (defun sp--org-skip-asterisk (ms mb me)
    (or (and (= (line-beginning-position) mb)
             (eq 32 (char-after (1+ mb))))
        (and (= (1+ (line-beginning-position)) me)
             (eq 32 (char-after me)))))

  ;; Org-mode
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "*"
                   :actions '(insert wrap)
                   :unless '(sp-point-after-word-p sp-point-at-bol-p)
                   :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
    (sp-local-pair "/" "/" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p)
                   :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»"))
)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
