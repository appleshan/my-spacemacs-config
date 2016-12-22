;;; funcs.el --- appleshan-appearance Layer functions File for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License: GPLv3

;;; Code:

;; @see https://github.com/evacchi/tabbar-layer/blob/master/packages.el
(defun tabbar-theme-setup ()
  (set-face-attribute
    'tabbar-default nil
    :background "gray20"
    :foreground "gray20"
    :box '(:line-width 1 :color "gray20" :style nil))
  (set-face-attribute
    'tabbar-unselected nil
    :background "gray30"
    :foreground "white"
    :box '(:line-width 3 :color "gray30" :style nil))
  (set-face-attribute
    'tabbar-selected nil
    :background "gray75"
    :foreground "purple"
    :box '(:line-width 3 :color "gray75" :style nil))
  (set-face-attribute
    'tabbar-highlight nil
    :background "white"
    :foreground "purple"
    :underline nil
    :box '(:line-width 3 :color "white" :style nil))
  (set-face-attribute
    'tabbar-button nil
    :box '(:line-width 1 :color "gray20" :style nil))
  (set-face-attribute
    'tabbar-separator nil
    :background "gray20"
    :height 0.6)

  ;; Change padding of the tabs
  ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
  (setq tabbar-separator '(0.6))
  ;; the color of the tabbar background
  (setq tabbar-background-color "#001214")
)

;; adding spaces
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(defun switch-tabbar (num)
  (let* ((tabs (tabbar-tabs
                (tabbar-current-tabset)
                ;; (tabbar-get-tabset "All Buffers")
                ))
         (tab (nth
               (if (> num 0) (- num 1) (+ (length tabs) num))
               tabs)))
    (if tab (switch-to-buffer (car tab)))))

(global-set-key (kbd "C-1") (lambda () (interactive) (switch-tabbar 1)))
(global-set-key (kbd "C-2") (lambda () (interactive) (switch-tabbar 2)))
(global-set-key (kbd "C-3") (lambda () (interactive) (switch-tabbar 3)))
(global-set-key (kbd "C-4") (lambda () (interactive) (switch-tabbar 4)))
(global-set-key (kbd "C-5") (lambda () (interactive) (switch-tabbar 5)))
(global-set-key (kbd "C-6") (lambda () (interactive) (switch-tabbar 6)))
(global-set-key (kbd "C-7") (lambda () (interactive) (switch-tabbar 7)))
(global-set-key (kbd "C-8") (lambda () (interactive) (switch-tabbar 8)))
(global-set-key (kbd "C-9") (lambda () (interactive) (switch-tabbar 9)))
(global-set-key (kbd "C-0") (lambda () (interactive) (switch-tabbar -1)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
