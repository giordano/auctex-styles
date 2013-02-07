;;; acro.el --- AUCTeX style for `acro.sty'.

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

;; This file adds support for `acro.sty'.

;;; Code:

(defvar LaTeX-acro-package-options
  '(;; General Options
    ("single" ("true" "false"))
    ("hyperref" ("true" "false"))
    ("only-used" ("true" "false"))
    ("macros" ("true" "false"))
    ("xspace" ("true" "false"))
    ("sort" ("true" "false"))
    ("cite" ("all" "first" "none"))
    ("cite-cmd")
    ("cite-space")
    ("uc-cmd")
    ;; Options Regarding Acronyms
    ("short-format")
    ("long-format")
    ("list-long-format")
    ("extra-format")
    ("first-style" ("default" "plain" "footnote" "square" "short" "reversed" "plain-reversed"))
    ("extra-style" ("default" "plain" "comma" "paren" "bracket"))
    ("plural-ending")
    ;; Options Regarding the List
    ("page-ref" ("none" "plain" "comma" "paren"))
    ("page-name")
    ("list-type" ("table" "itemize" "description"))
    ("list-style" ("list" "tabular" "longtable" "extra-tabular" "extra-longtable" "extra-tabular-rev" "extra-longtable-rev"))
    ("list-header" ("chapter" "chapter*" "section" "section*" "subsection" "subsection*"))
    ("list-name")
    ("list-table-width")
    ("list-caps" ("true" "false")))
  "Package options for the acro package.")

(TeX-auto-add-type "acronym" "LaTeX")

;; Self Parsing -- see (info "(auctex)Hacking the Parser").
(defvar LaTeX-acro-regexp
  '("\\\\DeclareAcronym\\*?{\\([^\n\r%\\{}]+\\)}" 1 LaTeX-auto-acro)
  "Matches acronym.")

(defvar LaTeX-auto-acro nil
  "Temporary for parsing acronym definitions.")

(defun LaTeX-acro-prepare ()
  "Clear `LaTex-auto-acro' before use."
  (setq LaTeX-auto-acro nil))

(defun LaTeX-acro-cleanup ()
  "Move symbols from `LaTeX-auto-acro' to `LaTeX-acro-list' and to
`TeX-auto-symbol' if option `macros' is set to `true'."
  (add-to-list 'LaTeX-acronym-list LaTeX-auto-acro)
  (when (or (member "macros" TeX-active-styles)
	    (member "macros=true" TeX-active-styles))
    (add-to-list 'LaTeX-acronym-list TeX-auto-symbol)))

;; FIXME: This does not seem to work unless one does a manual reparse.
(add-hook 'TeX-auto-prepare-hook 'LaTeX-acro-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-acro-cleanup)


(defun TeX-arg-acronym (optional &optional prompt definition)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen acronym to the
list of defined acronyms."
  (let ((acronym (completing-read (TeX-argument-prompt optional prompt "Acronym")
				  (LaTeX-acronym-list))))
    (if (and definition (not (string-equal "" acronym)))
	(LaTeX-add-acronyms acronym))
    (TeX-argument-insert acronym optional optional)))

(defun TeX-arg-define-acronym (optional &optional prompt)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-acronym optional prompt t))

(defun LaTeX-acronym-optional-argument (ignore)
  "Prompt for additional information for acronym,
insert surrounded by curly braces only if string is not of
zero length."
  (let ((addinfo (read-string "Additional information: ")))
    (unless (zerop (length addinfo))
      (insert TeX-grop addinfo TeX-grcl))))

(TeX-add-style-hook
 "acro"
 (lambda ()
   (TeX-auto-add-regexp LaTeX-acronym-regexp)
   (TeX-add-symbols
    ;; Creating new acronyms
    '("DeclareAcronym" TeX-arg-define-acronym "Short form, optional plural ending" [ "Alternative short form" ]
      "Long form, optional plural ending" LaTeX-acronym-optional-argument [ "Class" ])
    '("DeclareAcronym*" TeX-arg-define-acronym "Short form, optional plural ending" [ "Alternative short form" ]
      "Long form, optional plural form" LaTeX-acronym-optional-argument [ "Class" ])
    '("DeclareAcronymFormat" TeX-arg-acronym "Format")
    '("DeclareAcronymCitation" TeX-arg-acronym [ "Pre" ] [ "Post" ] TeX-arg-cite)
    '("DeclareAcronymPDFString" TeX-arg-acronym "PDF entry, optional plural ending")
    ;; Using the Acronyms
    '("ac" TeX-arg-acronym)
    '("ac*" TeX-arg-acronym)
    '("Ac" TeX-arg-acronym)
    '("Ac*" TeX-arg-acronym)
    '("acs" TeX-arg-acronym)
    '("acs*" TeX-arg-acronym)
    '("acl" TeX-arg-acronym)
    '("acl*" TeX-arg-acronym)
    '("aca" TeX-arg-acronym)
    '("aca*" TeX-arg-acronym)
    '("acf" TeX-arg-acronym)
    '("acf*" TeX-arg-acronym)
    '("Acf" TeX-arg-acronym)
    '("Acf*" TeX-arg-acronym)
    '("acp" TeX-arg-acronym)
    '("acp*" TeX-arg-acronym)
    '("Acp" TeX-arg-acronym)
    '("Acp*" TeX-arg-acronym)
    '("acsp" TeX-arg-acronym)
    '("acsp*" TeX-arg-acronym)
    '("aclp" TeX-arg-acronym)
    '("aclp*" TeX-arg-acronym)
    '("Aclp" TeX-arg-acronym)
    '("Aclp*" TeX-arg-acronym)
    '("acap" TeX-arg-acronym)
    '("acap*" TeX-arg-acronym)
    '("acfp" TeX-arg-acronym)
    '("acfp*" TeX-arg-acronym)
    '("Acfp" TeX-arg-acronym)
    '("Acfp*" TeX-arg-acronym)
    '("acreset" "List of acronyms")
    '("acresetall" 0)
    '("acuse" "List of acronyms")
    '("acuseall" 0)
    ;; PDF bookmarks
    '("acpdfstring" TeX-arg-acronym)
    '("acpdfstringplural" TeX-arg-acronym)
    ;; Printing the List
    '("printacronyms" "List of classes" [ "List of excluded classes" ])
    ;; Customization
    '("acsetup" (TeX-arg-key-val LaTeX-acro-package-options)))
   (TeX-run-style-hooks
    "l3sort"
    "xspace"
    "xtemplate"
    "l3keys2e"
    "xparse"
    "expl3")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(;; last two arguments of DeclareAcronym (should
				;; be "{[") are both optional, we don't need to
				;; explicit list them here
				("DeclareAcronym" "*{{[{")
				("DeclareAcronymFormat" "{{")
				("DeclareAcronymCitation" "{[[{")
				("DeclareAcronymPDFString" "{{")
				("ac" "*{")
				("Ac" "*{")
				("acs" "*{")
				("acl" "*{")
				("aca" "*{")
				("acf" "*{")
				("Acf" "*{")
				("acp" "*{")
				("Acp" "*{")
				("acsp" "*{")
				("aclp" "*{")
				("Aclp" "*{")
				("acap" "*{")
				("acfp" "*{")
				("Acfp" "*{")
				("acuse" "{"))
			      'function))))

;;; acro.el ends here
