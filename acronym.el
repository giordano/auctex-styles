;;; acronym.el --- AUCTeX style for `acronym.sty'.

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

;; This file adds support for `acronym.sty'.

;;; Code:

;; TODO: improve regexp, acronym may also be a macro
(setq LaTeX-auto-regexp-list
      (append
       '(("\\\\acro{\\([^\n\r%\\{}]+\\)}" 1 LaTeX-auto-acronym))
       LaTeX-auto-regexp-list))

(TeX-auto-add-type "acronym" "LaTeX")

(defun TeX-arg-acronym (optional &optional prompt definition)
  "Prompt for a acronym completing with known acronyms.
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
  "Prompt for a acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-acronym optional prompt t))

(TeX-add-style-hook "acronym"
 (lambda ()
    (LaTeX-add-environments
     '("acronym" LaTeX-env-args [TeX-arg-acronym]))
    (TeX-add-symbols
     '("ac" TeX-arg-acronym)
     '("acresetall" 0)
     '("acf" TeX-arg-acronym)
     '("acs" TeX-arg-acronym)
     '("acl" TeX-arg-acronym)
     '("acp" TeX-arg-acronym)
     '("acfp" TeX-arg-acronym)
     '("acsp" TeX-arg-acronym)
     '("aclp" TeX-arg-acronym)
     '("acfi" TeX-arg-acronym)
     '("acused" TeX-arg-acronym)
     '("acsu" TeX-arg-acronym)
     '("aclu" TeX-arg-acronym)
     '("iac" TeX-arg-acronym)
     '("Iac" TeX-arg-acronym)
     '("ac*" TeX-arg-acronym)
     '("acf*" TeX-arg-acronym)
     '("acs*" TeX-arg-acronym)
     '("acl*" TeX-arg-acronym)
     '("acp*" TeX-arg-acronym)
     '("acfp*" TeX-arg-acronym)
     '("acsp*" TeX-arg-acronym)
     '("aclp*" TeX-arg-acronym)
     '("acfi*" TeX-arg-acronym)
     '("acsu*" TeX-arg-acronym)
     '("aclu*" TeX-arg-acronym)
     '("iac*" TeX-arg-acronym)
     '("Iac*" TeX-arg-acronym)
     '("acsfont" 1)
     '("acffont" 1)
     '("acfsfont" 1)
     '("acro" TeX-arg-define-acronym [ "Short name" ] "Full name")
     '("acroextra" "Additional info")
     '("newacro" TeX-arg-define-acronym [ "Short name" ] "Full name")
     '("acrodef" TeX-arg-define-acronym [ "Short name" ] "Full name")
     '("acroindefinite" TeX-arg-acronym "Short indefinite article" "Long indefinite article")
     '("acrodefindefinite" TeX-arg-acronym "Short indefinite article" "Long indefinite article")
     '("newacroindefinite" TeX-arg-acronym "Short indefinite article" "Long indefinite article")
     '("acroplural" TeX-arg-acronym [ "Short plural" ] "Long plural")
     '("acrodefplural" TeX-arg-acronym [ "Short plural" ] "Long plural")
     '("newacroplural" TeX-arg-acronym [ "Short plural" ] "Long plural"))
    (TeX-run-style-hooks
     "relsize"
     "xstring"
     "suffix")
    ;; Fontification
    (when (and (featurep 'font-latex)
	       (eq TeX-install-font-lock 'font-latex-setup))
      (font-latex-add-keywords '(("ac" "*{")
				 ("acf" "*{")
				 ("acs" "*{")
				 ("acl" "*{")
				 ("acp" "*{")
				 ("acfp" "*{")
				 ("acsp" "*{")
				 ("aclp" "*{")
				 ("acfi" "*{")
				 ("acused" "*{")
				 ("aclu" "*{")
				 ("iac" "*{")
				 ("Iac" "*{")
				 ("acro" "{[{")
				 ("acroextra" "{{")
				 ("newacro" "{[{")
				 ("acrodef" "{[{")
				 ("acroindefinite" "{{{")
				 ("acrodefindefinite" "{{{")
				 ("newacroindefinite" "{{{")
				 ("acroplural" "{[{")
				 ("acrodefplural" "{[{")
				 ("newacroplural" "{[{"))
   			      'function))))

(defvar LaTeX-acronym-package-options
  '("footnote" "nohyperlinks" "printonlyused" "withpage" "smaller" "dua" "nolist")
  "Package options for the acronym package.")

;; acronym.el ends here
