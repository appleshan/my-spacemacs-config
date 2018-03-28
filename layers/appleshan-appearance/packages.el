;;; packages.el --- appleshan-appearance Layer packages File for Spacemacs
;;
;; Copyright (c) 2016-2020 Apple Shan
;;
;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq appleshan-appearance-packages
    '(
      anzu
      all-the-icons
      beacon
      on-screen
      paren-face
      popwin
      visual-regexp
      visual-regexp-steroids
      which-key
      spacemacs-whitespace-cleanup
      zoom
      ))

;; List of packages to exclude.
(setq appleshan-appearance-excluded-packages '())

;; Show number of matches in mode-line while searching
(defun appleshan-appearance/post-init-anzu ()
  (setq anzu-replace-to-string-separator
        (if (char-displayable-p ?→) " → " " -> ")))

(defun appleshan-appearance/init-all-the-icons ()
  (use-package all-the-icons))

;; 不会丢失你的光标
(defun appleshan-appearance/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "tb")
      (spacemacs/toggle-beacon-on))
    :config
    (spacemacs|hide-lighter beacon-mode)))

(defun appleshan-appearance/init-on-screen ()
  (use-package on-screen
    :init (require 'on-screen)
    :config (on-screen-global-mode +1)))

(defun appleshan-appearance/init-paren-face ()
  (use-package paren-face
    :ensure t
    :init (global-paren-face-mode)))

;; Popup Window Manager
(defun appleshan-appearance/post-init-popwin ()
  ;; man
  (push '(Man-mode :stick t :height 20) popwin:special-display-config)

  ;; Elisp
  (push '("*ielm*" :stick t) popwin:special-display-config)

  ;; python
  (push '("*Python*"   :stick t) popwin:special-display-config)
  (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)

  ;; prodigy
  (push '("*prodigy*" :stick t) popwin:special-display-config)

  ;; org-mode
  (push '("*Org tags*" :stick t :height 30) popwin:special-display-config)

  ;; Completions
  (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

  ;; I recommend you set popwin for quickrun.el
  ;; See also http://www.emacswiki.org/emacs/PopWin
  (push '("*quickrun*") popwin:special-display-config)

  (push "*appleshan/run-current-file output*" popwin:special-display-config)
  (delete "*Async Shell Command*" popwin:special-display-config))

;; 可视化正则匹配
;; https://github.com/benma/visual-regexp.el
;; https://github.com/benma/visual-regexp-steroids.el

(defun appleshan-appearance/init-visual-regexp ()
  (use-package visual-regexp
    ; :commands (vr/replace vr/query-replace) ; See the bind of init-visual-regexp-steroids.
    :defer t))

(defun appleshan-appearance/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :defer t
    :bind (("C-M-%" . vr/replace)
           ("M-%"   . vr/query-replace)
           ("C-M-r"   . vr/isearch-backward)
           ("C-M-s"   . vr/isearch-forward)
           ("C-s" . isearch-forward)  ; ordinary forward search
           ("C-r" . isearch-backward) ; ordinary backward search
           ("C-c m" . vr/mc-mark)  ; for multiple-cursors
           )))

(defun appleshan-appearance/post-init-which-key ()
  (spacemacs|diminish which-key-mode))

(defun appleshan-appearance/post-init-spacemacs-whitespace-cleanup ()
  (spacemacs|diminish spacemacs-whitespace-cleanup-mode))

(defun appleshan-appearance/init-zoom ()
  (use-package zoom
    :config
    (progn
      ;; Resize the selected window using the golden ratio:
      ; (setq zoom-size '(0.618 . 0.618))

      ;; Resize the selected window according to the frame width, for example:
      ;; 90 columns and 75% of the frame height if the frame width is larger than 1024 pixels;
      ;; half the frame size otherwise.
      (defun size-callback ()
        (cond ((> (frame-pixel-width) 1280) '(90 . 0.75))
              (t                            '(0.5 . 0.5))))
      (setq zoom-size 'size-callback)

      (setq zoom-ignored-major-modes '(dired-mode org-mode magit-mode))
      (setq zoom-ignored-buffer-name-regexps '("^*calc"))
      ;; 少于20行的任何缓冲区。
      (setq zoom-ignore-predicates '(
        (lambda () (> (count-lines (point-min) (point-max)) 20))))
      (zoom-mode t))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; packages.el ends here
