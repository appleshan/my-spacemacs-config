;;; config.el --- appleshan-chinese Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar *large-buffer-threshold* 300000
  "Buffer whose size beyond it will have a different behavior for the efficiency")

(defvar chinese-default-input-method 'pinyin
  "The default chinese input method. Can be `wubi` or `pinyin`.")

(defvar chinese-enable-avy-pinyin t
  "Enable ace-pinyin in avy-goto-char")

(defvar chinese-enable-fcitx nil
  "Enable fcitx to help writing Chinese in Evil mode.")

;; Set the monospaced font size when mixed Chinese and English words
(defun spacemacs//set-monospaced-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: nil
;; End:

;;; config.el ends here
