;;; polyglossia.el --- AUCTeX style for `polyglossia.sty' version 1.2.1.

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

;; This file adds support for `polyglossia.sty' version 1.2.1.

;;; TODO:

;; -- Create language specific styles with names `gloss-<lang>.el'.  They should
;;    add `text<lang>' macros, `<lang>' environments (`Arabic' for `arabic'
;;    language), and the others language-specific commands.

;;; Code:

(TeX-auto-add-type "polyglossia-lang" "LaTeX")

;; Self Parsing -- see (info "(auctex)Hacking the Parser").
(defvar LaTeX-polyglossia-lang-regexp
  (concat "\\\\set\\(defaultlanguage\\|mainlanguage\\|otherlanguages?\\)"
	  "[ \t\n\r]*\\(?:\\[\\(.*\\)\\]\\)?[ \t\n\r]*{\\([A-Za-z, ]+\\)}")
  "Matches languages set with polyglossia macros.")

(defvar LaTeX-polyglossia-setkeys-regexp
  (concat "\\\\setkeys"
	  "[ \t\n\r]*{\\([A-Za-z]+\\)}[ \t\n\r]*{\\([^}]\\)}")
  "Matches polyglossia languages options set using `\setkeys'.")

(defvar LaTeX-auto-polyglossia-lang nil
  "Temporary for parsing polyglossia languages.")

(defvar LaTeX-auto-polyglossia-setkeys nil
  "Temporary for parsing polyglossia language options.")

(defun LaTeX-polyglossia-prepare ()
  "Clear `LaTex-auto-polyglossia-lang' before use."
  (setq LaTeX-auto-polyglossia-lang nil
	LaTeX-auto-polyglossia-setkeys nil))

(defun LaTeX-polyglossia-cleanup ()
  "Move languages and their options from
`LaTeX-auto-polyglossia-lang' to `LaTeX-polyglossia-lang-list'."
  ;; Examle: now the value of `LaTeX-auto-polyglossia-lang' is something like
  ;;   '(("danish" "defaultlanguage" "")
  ;;     ("arabic" "otherlanguage" "locale=tunisia,numerals=maghrib")
  ;;     ("german" "otherlanguage" "spelling=new,script=latin")
  ;;     ("icelandic,brazil,sanskrit" "otherlanguages" ""))
  ;; We want to end up with a list like
  ;;   '(("danish" "defaultlanguage")
  ;;     ("arabic" "otherlanguage" "locale=tunisia" "numerals=maghrib")
  ;;     ("german" "otherlanguage" "spelling=new" "script=latin")
  ;;     ("icelandic" "otherlanguages")
  ;;     ("brazil" "otherlanguages")
  ;;     ("sanskrit" "otherlanguages" "script=Devanagari"))
  ;; with `script=Devanagari' option to `sanskrit' language set using
  ;; `\setkeys'.
  ;; In each element of the alist, the key is the language, the second value is
  ;; the polyglossia command which set the language, the rest of values is the
  ;; list of options given to the language.
  (let (tmp newelt opts otheropts)
    (dolist (elt LaTeX-auto-polyglossia-lang)
      (mapc
       (lambda (language)
	 ;; `opts' is the string of options for `language', set using
	 ;; `\setdefaultlanguage' or `\setotherlanguage'.
	 (setq opts (cdr (cdr elt)))
	 ;; `otheropts' is the string of options for `language' set using
	 ;; `\setkeys'.
	 (setq otheropts
	       (car (cdr (assoc language LaTeX-auto-polyglossia-setkeys))))
	 (setq newelt
	       (append
		(list language) (list (nth 1 elt))
		(unless (equal opts '(""))
		  (LaTeX-listify-package-options (car opts)))
		(if otheropts (LaTeX-listify-package-options otheropts))))
	 (add-to-list 'LaTeX-polyglossia-lang-list newelt t)
	 (add-to-list 'tmp newelt t))
       (LaTeX-listify-package-options (car elt))))
    (setq LaTeX-auto-polyglossia-lang tmp)))

;; FIXME: This does not seem to work unless one does a manual reparse.
(add-hook 'TeX-auto-prepare-hook 'LaTeX-polyglossia-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-polyglossia-cleanup)

(defvar LaTeX-polyglossia-language-list
  '("albanian" "amharic" "arabic" "armenian" "asturian" "bahasai" "bahasam"
    "basque" "bengali" "brazil" "breton" "bulgarian" "catalan" "coptic"
    "croatian" "czech" "danish" "divehi" "dutch" "english" "esperanto"
    "estonian" "farsi" "finnish" "french" "galician" "german" "greek" "hebrew"
    "hindi" "icelandic" "interlingua" "irish" "italian" "lao" "latin" "latvian"
    "lithuanian" "lsorbian" "magyar" "malayalam" "marathi" "norsk" "nynorsk"
    "occitan" "polish" "portuges" "romanian" "russian" "samin" "sanskrit"
    "scodish" "serbian" "slovak" "slovenian" "spanish" "swedish" "syriac"
    "tamil" "telugu" "thai" "turkish" "turkmen" "ukrainian" "urdu" "usorbian"
    "vietnamese" "welsh")
  "List of languages supported by the polyglossia LaTeX package.")

(defun LaTeX-polyglossia-active-languages ()
  "Return a list of polyglossia languages used in the document.
The last language is the default one."
  (let (active-languages default)
    (dolist (elt (LaTeX-polyglossia-lang-list))
      (setq default (or (string-equal "defaultlanguage" (nth 1 elt))
			(string-equal "mainlanguage" (nth 1 elt))))
      ;; Append the language to the list if it's the default one.
      (add-to-list 'active-languages (car elt) default))
    active-languages))

(defun LaTeX-polyglossia-lang-option-member (language option)
  "Return non-nil if OPTION has been given to polyglossia LANGUAGE.
The value is actually the tail of the list of options given to LANGUAGE."
  (member option (cdr (cdr (assoc language (LaTeX-polyglossia-lang-list))))))

(defun LaTeX-arg-polyglossia-lang (optional default multiple setkeys)
  "Prompt for language and its options with completion and insert them
as arguments.

This function is triggered by `\setdefaultlanguage',
`\setotherlanguage', `\setotherlanguages', and `\setkeys' macros
by polyglossia package.

OPTIONAL is ignored, if DEFAULT is non-nil treat inserted
language as default, if MULTIPLE is non-nil prompt for multiple
languages, if SETKEYS is non-nil insert options as second
mandatory argument."
  ;; DEFAULT =  t , MULTIPLE = nil, SETKEYS = nil: `\setdefaultlanguage'.
  ;; DEFAULT = nil, MULTIPLE = nil, SETKEYS = nil: `\setotherlanguage'.
  ;; DEFAULT = nil, MULTIPLE =  t , SETKEYS = nil: `\setotherlanguages'.
  ;; DEFAULT = nil, MULTIPLE = nil, SETKEYS =  t : `\setkeys'.
  (let* ((function (if multiple 'TeX-completing-read-multiple 'completing-read))
	 (language (funcall function (if multiple "Languages: " "Language: ")
			    LaTeX-polyglossia-language-list))
	 var options)
    (if multiple
	(mapc (lambda (elt) (TeX-run-style-hooks (concat "gloss-" elt)))
	      language)
      (TeX-run-style-hooks (concat "gloss-" language)))
    ;; `\setotherlanguages' doesn't take options, don't prompt for them.
    (setq options
	  (if multiple ""
	    (setq var (intern (format "LaTeX-polyglossia-%s-options-list" language)))
	    (if (and (boundp var) (symbol-value var))
		;; `\setdefaultlanguage' and `\setotherlanguage' use `options'
		;; as first optional argument; `\setkeys' uses `options' as
		;; second mandatory argument.
		(TeX-read-key-val (not setkeys) (symbol-value var))
	      ;; When `LaTeX-polyglossia-<lang>-options-list' is nil or not
	      ;; defined, don't prompt for options.
	      "")))
    (unless setkeys
      (let ((TeX-arg-opening-brace LaTeX-optop)
	    (TeX-arg-closing-brace LaTeX-optcl))
	(TeX-argument-insert options t)))
    (if multiple
	(setq language (mapconcat 'identity language ",")))
    (TeX-argument-insert language nil)
    (if setkeys
	(TeX-argument-insert options nil))))

(defun LaTeX-arg-polyglossiasetup-options (optional)
  "Prompt for setup options of polyglossia package.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one."
  (TeX-arg-key-val optional
		   '(("language") ;; TODO: add completion in `fontspec.el', see
				  ;; `fontspec-xetex.sty' starting from line 1844.
		     ("hyphennames")
		     ("script") ;; TODO: add completion in `fontspec.el', see
				;; `fontspec-xetex.sty' starting from line 1802.
		     ("direction" ("RL" "LR"))
		     ("scripttag")
		     ("langtag")
		     ("hyphenmins")
		     ("frenchspacing" ("true" "false"))
		     ("indentfirst" ("true" "false"))
		     ("fontsetup" ("true" "false"))
		     ;; The following options aren't already implemented but are
		     ;; present in `polyglossia.sty' comments.
		     ;; ("nouppercase" ("true" "false"))
		     ;; ("localalph")
		     ;; ("localnumber")
		     )))

(TeX-add-style-hook
 "polyglossia"
 (lambda ()
   (TeX-auto-add-regexp
    `(,LaTeX-polyglossia-lang-regexp (3 1 2) LaTeX-auto-polyglossia-lang))
   (TeX-auto-add-regexp
    `(,LaTeX-polyglossia-setkeys-regexp (1 2) LaTeX-auto-polyglossia-setkeys))
   ;; Run style hooks for every active language.
   (mapc (lambda (elt) (TeX-run-style-hooks (concat "gloss-" elt)))
	 (LaTeX-polyglossia-active-languages))
   (TeX-run-style-hooks "etoolbox" "makecmds" "xkeyval" "fontspec")
   (TeX-add-symbols
    ;; Double `ignore's at the end of the following definitions is needed to
    ;; fool `TeX-auto-generate'd style file of `polyglossia.sty', which
    ;; otherwise would override these macro definitions.
    '("setdefaultlanguage" (LaTeX-arg-polyglossia-lang  t  nil nil) (ignore) (ignore))
    '("setmainlanguage"    (LaTeX-arg-polyglossia-lang  t  nil nil) (ignore) (ignore))
    '("setotherlanguage"   (LaTeX-arg-polyglossia-lang nil nil nil) (ignore) (ignore))
    '("setotherlanguages"  (LaTeX-arg-polyglossia-lang nil  t  nil) (ignore) (ignore))
    '("setkeys"            (LaTeX-arg-polyglossia-lang nil nil  t ) (ignore) (ignore))
    '("PolyglossiaSetup" (TeX-arg-key-val LaTeX-polyglossia-lang-list)
      LaTeX-arg-polyglossiasetup-options))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("setdefaultlanguage" "[{")
				("setmainlanguage" "[{")
				("setotherlanguage" "[{")
				("setotherlanguages" "{")
				("setkeys" "{{"))
			      'function))))

;; TODO: move each option variable in its specific `gloss-*.el' file.
(defvar LaTeX-polyglossia-arabic-options-list
  '(("calendar" ("gregorian" "islamic"))
    ("locale" ("default" "mashriq" "libya" "algeria" "tunisia" "morocco" "mauritania"))
    ("numerals" ("mashriq" "maghrib"))
    ("abjadjimnotail" ("false" "true")))
  "Arabic language options for the polyglossia package.")

(defvar LaTeX-polyglossia-bengali-options-list
  '(("numerals" ("Western" "Devanagari")))
  "Bengali language options for the polyglossia package.")

(defvar LaTeX-polyglossia-bengali-options-list
  '(("numerals" ("Western" "Devanagari")))
  "Bengali language options for the polyglossia package.")

(defvar LaTeX-polyglossia-catalan-options-list
  '(("babelshorthands" ("true" "false")))
  "Catalan language options for the polyglossia package.")

(defvar LaTeX-polyglossia-dutch-options-list
  '(("babelshorthands" ("true" "false")))
  "Dutch language options for the polyglossia package.")

(defvar LaTeX-polyglossia-english-options-list
  '(("variant" ("american" "usmax" "british" "australian" "newzealand"))
    ("ordinalmonthday" ("true" "false")))
  "English language options for the polyglossia package.")

(defvar LaTeX-polyglossia-farsi-options-list
  '(("numerals" ("western" "eastern"))
    ;; ("locale") ;; not yet implemented
    ;; ("calendar") ;; not yet implemented
    )
  "Farsi language options for the polyglossia package.")

(defvar LaTeX-polyglossia-german-options-list
  '(("spelling" ("new" "old"))
    ("latesthyphen" ("true" "false"))
    ("babelshorthands" ("true" "false"))
    ("script" ("latin" "fraktur")))
  "German language options for the polyglossia package.")

(defvar LaTeX-polyglossia-greek-options-list
  '(("variant" ("monotonic" "polytonic" "ancient"))
    ("numerals" ("greek" "arabic"))
    ("attic" ("true" "false")))
  "Greek language options for the polyglossia package.")

(defvar LaTeX-polyglossia-hebrew-options-list
  '(("numerals" ("hebrew" "arabic"))
    ("calendar" ("hebrew" "gregorian")))
  "Hebrew language options for the polyglossia package.")

(defvar LaTeX-polyglossia-hindi-options-list
  '(("numerals" ("Western" "Devanagari")))
  "Hindi language options for the polyglossia package.")

(defvar LaTeX-polyglossia-italian-options-list
  '(("babelshorthands" ("true" "false")))
  "Italian language options for the polyglossia package.")

(defvar LaTeX-polyglossia-lao-options-list
  '(("numerals" ("lao" "arabic")))
  "Lao language options for the polyglossia package.")

(defvar LaTeX-polyglossia-russian-options-list
  '(("spelling" ("modern" "old"))
    ("babelshorthands" ("true" "false")))
  "Russian language options for the polyglossia package.")

(defvar LaTeX-polyglossia-sanskrit-options-list
  '(("Script" ("Devanagari")))
  "Sanskrit language options for the polyglossia package.")

(defvar LaTeX-polyglossia-serbian-options-list
  '(("script" ("cyrillic" "latin")))
  "Serbian language options for the polyglossia package.")

(defvar LaTeX-polyglossia-syriac-options-list
  '(("numerals" ("western" "eastern" "abjad")))
  "Syriac language options for the polyglossia package.")

(defvar LaTeX-polyglossia-thai-options-list
  '(("numerals" ("thai" "arabic")))
  "Thai language options for the polyglossia package.")

(defvar LaTeX-polyglossia-package-options
  '("babelshorthands" "localmarks" "nolocalmarks" "quiet")
  "Package options for the polyglossia package.")

;;; polyglossia.el ends here