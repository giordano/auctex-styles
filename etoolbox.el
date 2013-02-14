;;; etoolbox.el --- AUCTeX style for `etoolbox.sty' version 2.1.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <giordano.mose@libero.it>
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `etoolbox.sty' version 2.1.

;;; Code:

;; Self Parsing -- see (info "(auctex)Hacking the Parser").
(defvar LaTeX-etoolbox-optional-regexp
  (concat "\\\\\\(?:new\\|renew\\|provide\\)?robustcmd\\*?{?\\\\\\("
	  TeX-token-char "+\\)}?\\[\\([0-9]+\\)\\]\\[\\([^\n\r]*\\)\\]")
  "Matches new etoolbox macro with optional arguments definitions.")

(defvar LaTeX-etoolbox-arguments-regexp
  (concat "\\\\\\(?:new\\|renew\\|provide\\)?robustcmd\\*?{?\\\\\\("
	  TeX-token-char "+\\)}?\\[\\([0-9]+\\)\\]")
  "Matches new etoolbox macro with arguments definitions.")

(defvar LaTeX-etoolbox-macro-regexp
  (concat "\\\\\\(?:new\\|renew\\|provide\\)?robustcmd\\*?{?\\\\\\("
	  TeX-token-char "+\\)}?")
  "Matches new etoolbox macro definitions.")

(defvar LaTeX-auto-etoolbox-optional nil
  "Temporary for parsing etoolbox macro with optional arguments definitions.")

(defvar LaTeX-auto-etoolbox-arguments nil
  "Temporary for parsing etoolbox macro with arguments definitions.")

(defvar LaTeX-auto-etoolbox-macro nil
  "Temporary for parsing etoolbox macro definitions.")

(defun LaTeX-etoolbox-prepare ()
  "Clear `LaTex-auto-etoolbox' before use."
  (setq LaTeX-auto-etoolbox-optional nil
	LaTeX-auto-etoolbox-arguments nil
	LaTeX-auto-etoolbox-macro nil))

(defun LaTeX-etoolbox-cleanup ()
  "Move symbols from `LaTeX-auto-etoolbox-*' to `TeX-auto-symbol'."

  ;; Macros with only an argument
  (mapcar (lambda (symbol)
  	    (add-to-list 'TeX-auto-symbol symbol))
  	  LaTeX-auto-etoolbox-macro)

  ;; Macros with arguments
  (mapc (lambda (entry)
	  (add-to-list 'TeX-auto-symbol
		       (list (nth 0 entry)
			     (string-to-number (nth 1 entry)))))
	LaTeX-auto-etoolbox-arguments)

  ;; Macros with optional argument
  (mapc (lambda (entry)
	  (add-to-list 'TeX-auto-symbol
		       (list (nth 0 entry)
			     (vector "argument")
			     (1- (string-to-number (nth 1 entry))))))
	LaTeX-auto-etoolbox-optional))

;; FIXME: This does not seem to work unless one does a manual reparse.
(add-hook 'TeX-auto-prepare-hook 'LaTeX-etoolbox-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-etoolbox-cleanup)

(TeX-add-style-hook
 "etoolbox"
 (lambda ()
   (TeX-auto-add-regexp `(,LaTeX-etoolbox-macro-regexp 1 LaTeX-auto-etoolbox-macro))
   (TeX-auto-add-regexp `(,LaTeX-etoolbox-arguments-regexp (1 2) LaTeX-auto-etoolbox-arguments))
   (TeX-auto-add-regexp `(,LaTeX-etoolbox-optional-regexp (1 2 3) LaTeX-auto-etoolbox-optional))
   (TeX-run-style-hooks "etex")
    (TeX-add-symbols
     ;; TODO: add symbols according to `etoolbox' manual.  Here is an automatic
     ;; genereted list using `TeX-auto-generate'.
     '("forlistcsloop" 2)
     '("forlistloop" 2)
     '("ifrmnum" 1)
     '("rmntonum" 1)
     '("nottoggle" 1)
     '("iftoggle" 1)
     '("notbool" 1)
     '("ifbool" 1)
     '("csuse" 1)
     '("csexpandonce" 1)
     '("expandonce" 1)
     '("ifboolexpe" 1)
     '("ifdimless" 1)
     '("ifdimgreater" 1)
     '("ifdimequal" 1)
     '("ifdimcomp" 3)
     '("ifnumodd" 1)
     '("ifnumless" 1)
     '("ifnumgreater" 1)
     '("ifnumequal" 1)
     '("ifnumcomp" 3)
     '("notblank" 1)
     '("ifblank" 1)
     '("ifstrempty" 1)
     '("ifcsdimen" 1)
     '("ifdefdimen" 1)
     '("ifcslength" 1)
     '("ifdeflength" 1)
     '("ifltxcounter" 1)
     '("ifcscounter" 1)
     '("ifdefcounter" 1)
     '("ifcsstrequal" 2)
     '("ifcsequal" 2)
     '("ifdefequal" 2)
     '("ifcsvoid" 1)
     '("ifdefvoid" 1)
     '("ifcsempty" 1)
     '("ifdefempty" 1)
     '("ifcsprotected" 1)
     '("ifcsparam" 1)
     '("ifcsprefix" 1)
     '("ifdefprefix" 1)
     '("ifcsmacro" 1)
     '("ifcsundef" 1)
     '("ifcsdef" 1)
     '("ifundef" 1)
     '("ifdef" 1)
     "ifdefmacro"
     "ifdefparam"
     "ifdefprotected"
     "etb"
     "dolistloop"
     "dolistcsloop"
     "newrobustcmd"
     "protecting"
     "protect"
     "relax"
     "string"
     "tracingpatches"
     "noexpand"
     "listbreak"
     "AtEndPreamble"
     "AfterPreamble"
     "AfterEndPreamble"
     "AfterEndDocument")))

(defvar LaTeX-etoolbox-package-options nil
  "Package options for the etoolbox package.")

;; etoolbox.el ends here
