;;; kpfonts.el --- AUCTeX style for `kpfonts.sty' version 3.31.

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

;; This file adds support for `kpfonts.sty' version 3.31.

;;; Code:

;; New fonts by `kpfonts'.
(setq TeX-font-list
      (append
       LaTeX-font-list
       '(;; Math fonts
	 (?\C-h "" "" "\\mathupright{" "}")
	 (?\C-k "" "" "\\mathfrak{"    "}")
	 (?\C-p "" "" "\\mathscr{"     "}")
	 ;; Text fonts
	 (?\C-l "\\textscsl{"      "}")
	 (?\C-o "\\textothersc{"   "}")
	 (?\C-q "\\textotherscsl{" "}"))))

(TeX-add-style-hook
 "kpfonts"
 (lambda ()
   (unless (member "notextcomp" TeX-active-styles)
     (TeX-run-style-hooks "full" "textcomp"))
   (unless (member "noamsmath" TeX-active-styles)
     (TeX-run-style-hooks "amsmath"))
   (when (member "easyscsl" TeX-active-styles)
     (TeX-run-style-hooks "ifthen"))
   (TeX-add-symbols
    ;; Math fonts
    '("mathup" 1)
    '("mathupright" 1)
    ;; Text fonts options
    '("classicstylenums" 1)
    ;; New text commands
    '("scslshape" 0)
    '("otherscshape" 0)
    '("otherscslshape" 0)
    "othertailQ"
    "othertailscq"
    "othertailscslq"
    ;; Variant integrate symbols
    '("D" 1)
    "varint"
    "variint"
    "variiint"
    "variiiint"
    "varidotsint"
    ;; New extensive symbols
    "widearc"
    "widearcarrow"
    "wideOarc"
    "wideparen"
    '("widering" 1))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("textscsl" "{")
				("textothersc" "{")
				("textotherscsl" "{"))
			      'bold-command)
     (font-latex-add-keywords '(("textscsl" "{")
				("textotherscsl" "{"))
			      'italic-command)
     (font-latex-add-keywords '(("scslshape")
				("otherscshape")
				("otherscslshape"))
			      'bold-declaration)
     (font-latex-add-keywords '(("scslshape")
				("otherscslshape"))
			      'italic-declaration))))

(defvar LaTeX-kpfonts-package-options
  '(;; Main global options
    "light" "fulloldstylenums" "fulloldstyle" "fullveryoldstyle"
    ;; Other global options
    "nomath" "notext" "nosf" "nott" "onlyrm" "noamsmath" "notextcomp"
    ;; Text fonts options
    "lighttext" "oldstylenums" "oldstyle" "veryoldstyle" "rmx" "largesmallcaps"
    "easyscsl" "nofligatures" "lightmath"
    ;; Math typesetting options
    "sfmath" "sfmathbb" "rmmathbb" "nomathscript" "mathcalasscript" "classicReIm"
    "uprightRoman" "frenchstyle" "upright" "oldstylenumsmath" "oldstylemath"
    "veryoldstylemath" "narrowiints" "partialup" "widermath" "noDcommand"
    ;; Position of subscripts and superscripts
    "intlimits" "fullintlimits" "nointlimits" "sumlimits" "fullsumlimits"
    "nosumlimits"
    ;; Greek letters in math mode, options
    "uprightgreeks" "slantedGreeks"
    ;; Other `amsmath' options
    "namelimits" "nonamelimits" "leqno" "reqno" "centertags" "tbtags"
    ;; Misc
    "nowarning")
  "Package options for the kpfonts package.")

;; kpfonts.el ends here
