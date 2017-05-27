## What is Unicad? ##

Unicad is short for Universal Charset Auto Detector. It is an
Emacs-Lisp port of Mozilla Universal Charset Detector.

## What can Unicad do? ##

Unicad helps Emacs to guess the correct coding system when opening a
file.

## What languages and coding systems does Unicad support? ##

  * Chinese Simplified  (gb18030, gbk, gb2312, hz-gb-2312, iso-2022-cn)
  * Chinese Triditional (big5, gb18030, gbk, euc-tw)
  * Japanese (sjis, euc-jp, iso-2022-jp)
  * Korean (euc-kr, iso-2022-kr)
  * Greek (iso-8859-7, windows-1253)
  * Russian (iso-8859-5, windows-1251, koi8-r, ibm855)
  * Bulgarian (iso-8859-5, windows-1251)
  * Western European (latin-1)
  * Central European (latin-2)
  * Unicode (utf-16le, utf-16be with/without signature, utf-8)

## Who should use Unicad? ##

  1. **Emacs Users**. Unicad is an Emacs extension that written in Elisp. It is designed and works for Emacs.
  1. **Multilanguage Users**. If you are English only speakers, then there is no need to install Unicad on Emacs. Otherwise, if you need to read and edit files in multiple language and use different coding systems, Unicad will definitely help you in recognizing coding systems.
  1. Anyone who is tired of struggling with **garbled** text. Normal Text Editors are not so intelligent to choose correct coding system among various and complicated charsets. I suggest you try the most powerful text editor ever in the world - Emacs and Unicad.

## How to use Unicad? ##

[Download](http://code.google.com/p/unicad/downloads/list) the latest unicad.el, copy it to your Emacs load path (e.g.
site-lisp directory), and add the following line to your ~/.emacs:
```
(require 'unicad)
```

You may byte compile this file to speed up the charset detecting
process.


## What's the difference between Unicad and Mozilla Universal Charset Detector? ##

  * optimized for detecting shorter text files.
  * add support for Central European Languages, that use iso-8859-2 coding system.
  * add support for Traditional Chinese that uses gbk coding system.
  * add support for single byte only katakana that uses sjis coding system.
  * ability of detecting utf-16le and utf-16be without signature.