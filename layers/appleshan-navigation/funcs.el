;;; funcs.el --- appleshan-navigation Layer functions File for Spacemacs

;; Copyright (c) 2016-2020 Apple Shan

;; Author: Apple Shan <apple.shan@gmail.com>
;; URL: https://github.com/appleshan/my-spacemacs-config

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License: GPLv3

;;; Code:

;; @see https://github.com/emacs-china/emacsist/blob/master/articles/2016-11-07%E4%BD%A0%E8%83%BD%E6%83%B3%E5%88%B0%E7%9A%84%E5%87%A0%E4%B9%8E%E6%89%80%E6%9C%89%E5%85%B3%E4%BA%8E%E8%A1%8C%E7%9A%84%E6%93%8D%E4%BD%9C.org

;; 移动到行首/行尾

(defun c-move-forward-line ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (cond ((eolp) (f-skip-bol) (setq -move 1))
            (t (end-of-line) (setq -move 2)))
    (cond ((and (eolp) (not (bolp))) (beginning-of-line) (setq -move 0))
          ((>= (current-column) (f-skip-bol t)) (end-of-line) (setq -move 2))
          (t (f-skip-bol) (setq -move 1)))))

(defun c-move-backward-line ()
  (interactive)
  (let ((col (f-skip-bol t)))
    (if (eq major-mode 'org-mode)
        (cond ((and (<= (current-column) col) (not (= col 2)))
               (org-up-element) (skip-chars-forward -chars) (setq -move 1))
              (t (f-skip-bol) (setq -move 1)))
      (cond ((and (bolp) (not (eolp))) (end-of-line) (setq -move 2))
            ((<= (current-column) col) (beginning-of-line) (setq -move 0))
            (t (f-skip-bol) (setq -move 1))))))

(defvar -chars " \t")
(make-variable-buffer-local '-chars)

;; bol -> 0, skip-bol -> 1, eol -> 2
(defvar -move 0)
(make-variable-buffer-local '-move)

;; 判断当前光标是否位于 skip-bol 以及移动到此处的函数

(defun f-skip-bol (&optional save)
  (let ((col (save-excursion
               (beginning-of-line)
               (skip-chars-forward -chars)
               (current-column))))
    (unless save (move-to-column col)) col))

;; 优化原本的上下方向键：
;; 指定光标在上下移动的时候，保持在行首/行尾或者 skip-bol 这三个位置，
;; 或者执行正常的移动

(defun f-move-up-or-down (n)
  (unless (minibufferp)
    (cond ((and (= -move 2) (eolp))
           (next-line n) (end-of-line))
          ((and (= -move 1) (= (current-column) (f-skip-bol t)))
           (next-line n) (f-skip-bol))
          (t (next-line n) (setq -move 0)))
    (f-visual-mode)))

(defun c-move-down ()
  (interactive)
  (f-move-up-or-down 1))

(defun c-move-up ()
  (interactive)
  (f-move-up-or-down -1))

;; 光标在段落间移动

(defun c-paragraph-backward ()
  (interactive)
  (unless (minibufferp)
    (if (not (eq major-mode 'org-mode))
        (backward-paragraph)
      (org-backward-element)
      (skip-chars-forward -chars))
    (f-visual-mode)))

(defun c-paragraph-forward ()
  (interactive)
  (unless (minibufferp)
    (if (not (eq major-mode 'org-mode))
        (forward-paragraph)
      (org-forward-element)
      (skip-chars-forward -chars))
    (f-visual-mode)))

;; 交换两行/两段落

(defun c-transpose-lines-down ()
  (interactive)
  (unless (minibufferp)
    (delete-trailing-whitespace)
    (end-of-line)
    (unless (eobp)
      (forward-line)
      (unless (eobp)
        (transpose-lines 1)
        (forward-line -1)
        (end-of-line)))))

(defun c-transpose-lines-up ()
  (interactive)
  (unless (minibufferp)
    (delete-trailing-whitespace)
    (beginning-of-line)
    (unless (or (bobp) (eobp))
      (forward-line)
      (transpose-lines -1)
      (beginning-of-line -1))
    (skip-chars-forward -chars)))

(defun c-transpose-paragraphs-down ()
  (interactive)
  (unless (minibufferp)
    (let ((p nil))
      (delete-trailing-whitespace)
      (backward-paragraph)
      (when (bobp) (setq p t) (newline))
      (forward-paragraph)
      (unless (eobp) (transpose-paragraphs 1))
      (when p (save-excursion (goto-char (point-min)) (kill-line))))))

(defun c-transpose-paragraphs-up ()
  (interactive)
  (unless (or (minibufferp) (save-excursion (backward-paragraph) (bobp)))
    (let ((p nil))
      (delete-trailing-whitespace)
      (backward-paragraph 2)
      (when (bobp) (setq p t) (newline))
      (forward-paragraph 2)
      (transpose-paragraphs -1)
      (backward-paragraph)
      (when p (save-excursion (goto-char (point-min)) (kill-line))))))

;; other

(defun c-copy-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (unless (or (eobp) buffer-read-only) (newline)))
  (delete-trailing-whitespace)
  (kill-ring-save (point-min) (point-max))
  (unless (minibufferp) (message "Current buffer copied")))

(defun c-indent-paragraph ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (indent-region (region-beginning) (region-end))))

(defun c-kill-region ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)
    (back-to-indentation)))

(defun c-kill-ring-save ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (save-excursion
      (f-skip-bol)
      (kill-ring-save (point) (line-end-position)))
    (unless (minibufferp) (message "Current line copied"))))

(defun c-set-or-exchange-mark (arg)
  (interactive "P")
  (if (use-region-p) (exchange-point-and-mark)
    (set-mark-command arg)))

(defun c-toggle-comment (beg end)
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2))))
  (unless (minibufferp)
    (comment-or-uncomment-region beg end)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; funcs.el ends here
