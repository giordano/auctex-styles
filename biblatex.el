;;; biblatex.el --- AUCTeX style for `biblatex.sty' version 2.5.

;; Copyright (C) 2012-2013 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2012-11-14
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

;; This file adds support for `biblatex.sty' version 2.5.

;;; Code:

(defvar LaTeX-biblatex-addbibresource-options
  '(("label")
    ("location" ("local" "remote"))
    ("type" ("file"))
    ("datatype" ("bibtex" "ris" "zoterordfxml" "endnotexml")))
  "Key=value options for addbibresource macro of the biblatex package.")

(defun LaTeX-arg-addbibresource (optional &optional prompt)
  "Prompt for a BibLaTeX database file.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (let (files inputs database)
    (if LaTeX-using-Biber
	(setq files 'TeX-Biber-global-files
	      inputs 'biberinputs)
      (setq files 'BibTeX-global-files
	    inputs 'bibinputs))
    (setq files 'TeX-Biber-global-files
	  inputs 'biberinputs)
    (message "Searching for BibLaTeX files...")
    (or (symbol-value files)
	(set files (mapcar 'list (TeX-search-files-by-type
				  'biberinputs 'global t nil))))
    (setq database (completing-read
		    (TeX-argument-prompt optional prompt "BibLaTeX files")
		    (append (mapcar 'list (TeX-search-files-by-type
					   inputs 'local t nil))
			    (symbol-value files))))
    (LaTeX-add-bibliographies database)
    (TeX-argument-insert database optional)))

(TeX-add-style-hook
 "biblatex"
 (lambda ()
   ;; Biblatex uses as default backend biber, run it unless biblatex `backend'
   ;; option value is one of `bibtex', `bibtex8', `bibtexu'.
   (unless (or (member "backend=bibtex" TeX-active-styles)
	       (member "backend=bibtex8" TeX-active-styles)
	       (member "backend=bibtexu" TeX-active-styles))
     (setq LaTeX-using-Biber t))

   (TeX-run-style-hooks
    "etoolbox"
    "keyval"
    "kvoptions"
    "logreq"
    "ifthen"
    "url")
   (TeX-add-symbols
    '("addbibresource" [TeX-arg-key-val LaTeX-biblatex-addbibresource-options]
      LaTeX-arg-addbibresource))

   ;; TODO: add symbols according to `biblatex' manual.  Here is an automatic
   ;; genereted list using `TeX-auto-generate'.
   (TeX-add-symbols
    "endrefsegment"
    "bibendnotewrapper"
    "bibfootnotewrapper"
    "ResetDataInheritance"
    "mkbibendnotetext"
    "mkbibendnote"
    "mkbibfootnotetext"
    "mkbibfootnote"
    "brackettext"
    "parentext"
    "mkbibbold"
    "mkbibemph"
    "mkbibquote"
    "supercite"
    "textcite"
    "smartcite"
    "footcitetext"
    "footcite"
    "parencite"
    "cite"
    "citefield"
    "citelist"
    "citename"
    "DeclareCiteCommand"
    "mancite"
    "OnManualCitation"
    "InitializeCitationStyle"
    "AtNextCitekey"
    "AtNextCite"
    "AtEveryCitekey"
    "AtEveryCite"
    "addsectionbib"
    "addglobalbib"
    "refsegment"
    "newrefsegment"
    "refsection"
    "newrefsection"
    "AtEveryLositem"
    "AtBeginShorthands"
    "printshorthands"
    "AtEveryBibitem"
    "AtBeginBibliography"
    "bibbycategory"
    "bibbysegment"
    "bibbysection"
    "printbibliography"
    "printbibheading"
    "mkbibneutord"
    "mkbibfemord"
    "mkbibmascord"
    "finalandcomma"
    "bibdatedash"
    "bibrangessep"
    "bibrangedash"
    "mkbibrangecompextra"
    "mkbibrangeterseextra"
    "mkbibrangelongextra"
    "mkbibrangeshortextra"
    "mkbibrangecomp"
    "mkbibrangeterse"
    "mkbibrangelong"
    "mkbibrangeshort"
    "bibnamedelimi"
    "bibnamedelimd"
    "bibnamedelimc"
    "bibnamedelimb"
    "bibnamedelima"
    "bibinithyphendelim"
    "bibinitdelim"
    "bibinitperiod"
    "DeclareIndexFieldAlias"
    "DeclareFieldAlias"
    "DeclareIndexListAlias"
    "DeclareListAlias"
    "DeclareIndexNameAlias"
    "DeclareNameAlias"
    "DeclareIndexFieldFormat"
    "DeclareFieldFormat"
    "DeclareIndexListFormat"
    "DeclareListFormat"
    "DeclareIndexNameFormat"
    "DeclareNameFormat"
    "DeclareCaseLangs"
    "MakeSentenceCase"
    "mkfirstpage"
    "mkcomprange"
    "blx"
    "NumCheckSetup"
    "DeclarePageCommands"
    "DeclareRangeCommands"
    "DeclareRangeChars"
    "DeclareNumChars"
    "usebibmacro"
    "providebibmacro"
    "renewbibmacro"
    "newbibmacro"
    "citereset"
    "InitializeBibliographyStyle"
    "nohyphenation"
    "allowhyphens"
    "hyphenate"
    "nbhyphen"
    "hyphen"
    "noligature"
    "adddotspace"
    "addabthinspace"
    "addabbrvspace"
    "addhpthinspace"
    "addlpthinspace"
    "addhighpenspace"
    "addlowpenspace"
    "addnbthinspace"
    "addthinspace"
    "addnbspace"
    "addspace"
    "addslash"
    "midsentence"
    "bibsentence"
    "unspace"
    "lbx"
    "enquote"
    "textciquote"
    "textoiquote"
    "textcoquote"
    "textooquote"
    "initiquote"
    "initoquote"
    '("DeclareSortExclusion" 2)
    '("DeclareSortingScheme" 2)
    '("DeclareDataInheritance" 3)
    '("DeclareStyleSourcemap" 1)
    '("DeclareSourcemap" 1)
    '("DeclareNoinit" 1)
    '("DeclareNosort" 1)
    '("volcitecmd" 1)
    '("mknumalph" 1)
    '("blxmkbibnote" 2)
    '("mkbibsuperscript" 1)
    '("mkbibbrackets" 1)
    '("mkbibparens" 1)
    '("DeclareAutoPunctuation" 1)
    '("DeclareAutoCiteCommand" 1)
    '("DeclareMultiCiteCommand" 1)
    '("blxciteicmd" 1)
    '("blxcitecmd" 1)
    '("defbibentryset" 2)
    '("RequireCitationStyle" 1)
    '("bibliography" 1)
    '("addtocategory" 2)
    '("DeclareBibliographyCategory" 1)
    '("defbibcheck" 2)
    '("defbibfilter" 2)
    '("defbibnote" 1)
    '("defbibheading" 1)
    '("defbibenvironment" 4)
    '("DeclareNameInputHandler" 2)
    '("DeclareListInputHandler" 2)
    '("DeclareFieldInputHandler" 2)
    '("DeclareRedundantLanguages" 2)
    '("mkbibmonth" 1)
    '("mkbibordinal" 1)
    '("mkbibdateshort" 3)
    '("mkbibdatelong" 3)
    '("mkbibrangetruncextra" 2)
    '("mkbibrangefullextra" 2)
    '("mkbibrangetrunc" 2)
    '("mkbibrangefull" 2)
    '("DeclareLanguageMapping" 2)
    '("DefineHyphenationExceptions" 2)
    '("DefineBibliographyStrings" 2)
    '("UndefineBibliographyExtras" 2)
    '("DefineBibliographyExtras" 2)
    '("NewBibliographyString" 1)
    '("MakeCapital" 1)
    '("Rn" 1)
    '("RN" 1)
    '("restorebibmacro" 1)
    '("savebibmacro" 1)
    '("restorecommand" 1)
    '("savecommand" 1)
    '("DeclareEntryOption" 1)
    '("DeclareTypeOption" 1)
    '("DeclareBibliographyOption" 1)
    '("DeclareBibliographyAlias" 2)
    '("DeclareBibliographyDriver" 1)
    '("RequireBibliographyStyle" 1)
    '("textnohyphenation" 1)
    '("autocap" 1)
    '("DeclarePunctuationPairs" 2)
    '("DeclareQuotePunctuation" 1)
    '("DeclareCapitalPunctuation" 1)
    '("ExecuteBibliographyOptions" ["argument"] 1)
    '("DeclareLabelyear" ["argument"] 1)
    '("DeclareLabeltitle" ["argument"] 1)
    '("DeclareLabelname" ["argument"] 1)
    '("DeclarePresort" ["argument"] 1)
    '("DefaultInheritance" ["argument"] 1)
    '("DeclareLabelalphaTemplate" ["argument"] 1)
    '("DeclareDriverSourcemap" ["argument"] 1)
    '("AtDataInput" ["argument"] 0)
    '("ifnameformatundef" ["argument"] 1)
    '("iflistformatundef" ["argument"] 1)
    '("iffieldformatundef" ["argument"] 1)
    '("restorenameformat" ["argument"] 1)
    '("restorelistformat" ["argument"] 1)
    '("restorefieldformat" ["argument"] 1)
    '("savenameformat" ["argument"] 1)
    '("savelistformat" ["argument"] 1)
    '("savefieldformat" ["argument"] 1)
    '("RequireBiber" ["argument"] 0)
    "nolinkurl"
    "ppspace"
    "sqspace"
    "RNfont"
    "Rnfont"
    "bibleftparen"
    "bibrightparen"
    "bibleftbracket"
    "bibrightbracket"
    "isundefined"
    "thelisttotal"
    "thelistcount"
    "theliststart"
    "theliststop"
    "thecitetotal"
    "themulticitecount"
    "themulticitetotal"
    "theinstcount"
    "themaxnames"
    "theminnames"
    "themaxitems"
    "theminitems"
    "thecitecount"
    "theuniquelist"
    "theuniquename"
    "therefsection"
    "therefsegment"
    "themaxextratitle"
    "themaxextratitleyear"
    "themaxextrayear"
    "themaxextraalpha"
    "theabbrvpenalty"
    "thehighnamepenalty"
    "thelownamepenalty"
    "themaxparens"
    "theparenlevel"
    "frenchspacing"
    "nonfrenchspacing"
    "newblock"
    "pno"
    "ppno"
    "nopp"
    "psq"
    "psqq"
    "j"
    "MakeUppercase"
    "MakeLowercase"
    "currentfield"
    "currentname"
    "currentlist"
    "finentry"
    "i"
    "biblcstring"
    "biblclstring"
    "biblcsstring"
    "bibstring"
    "biblstring"
    "bibsstring"
    "bibucstring"
    "bibuclstring"
    "bibucsstring"
    "bibcpstring"
    "bibcplstring"
    "bibcpsstring"
    "bibname"
    "refname"
    "losname"
    "InheritBibliographyStrings"
    "DeclareBibliographyStrings"
    "InheritBibliographyExtras"
    "DeclareBibliographyExtras"
    "UndeclareBibliographyExtras"
    "DeclareHyphenationExceptions"
    "regexp"
    "item"
    "break"
    "NewCount"
    "NewOption"
    "NewValue"
    "preamble"
    "warn"
    "missing"
    "keyalias"
    "set"
    "inset"
    "keyw"
    "strng"
    "count"
    "true"
    "false"
    "key"
    "IeC"
    "makelabel"
    "not"
    "NOT"
    "and"
    "AND"
    "or"
    "OR"
    "section"
    "segment"
    "type"
    "subtype"
    "keyword"
    "category"
    "the"
    "skipentry"
    "nocite"
    "blxmciteicmd"
    "blxmcites"
    "blxendmcites"
    "ifhyperref"
    "ifmorenames"
    "ifmoreitems"
    "ifciteseen"
    "ifentryseen"
    "ifciteibid"
    "ifciteidem"
    "ifopcit"
    "ifloccit"
    "ifsamepage"
    "iffirstonpage"
    "ifcurrentfield"
    "ifcurrentlist"
    "ifcurrentname"
    "ifentrytype"
    "iffieldequalcs"
    "iffieldequals"
    "iffieldequalstr"
    "iffieldsequal"
    "iffieldundef"
    "ifnameequalcs"
    "ifnameequals"
    "ifnamesequal"
    "ifnameundef"
    "ifcategory"
    "ifkeyword"
    "ifentrycategory"
    "ifentrykeyword"
    "ifcapital"
    "ifinteger"
    "iffieldint"
    "ifnumeral"
    "ifnumerals"
    "ifpages"
    "iffieldnum"
    "iffieldnums"
    "iffieldpages"
    "ifbibstring"
    "iffieldbibstring"
    "ifnatbibmode"
    "ifcitation"
    "ifbibliography"
    "ifciteindex"
    "ifbibindex"
    "iffootnote"
    "ifuseprefix"
    "ifuseauthor"
    "ifuseeditor"
    "ifusetranslator"
    "ifterseinits"
    "iffirstinits"
    "ifsingletitle"
    "ifandothers"
    "noexpand"
    "labelalphaothers"
    "sortalphaothers"
    "protect"
    "nosort"
    "noinit"
    "maps"
    "map"
    "perdatasource"
    "pertype"
    "step"
    "labelelement"
    "except"
    "inherit"
    "noinherit"
    "sort"
    "name"
    "list"
    "literal"
    "citeorder"
    "field"
    "newblockpunct"
    "bibopenparen"
    "bibcloseparen"
    "bibopenbracket"
    "bibclosebracket")
   (LaTeX-add-environments
    "refsegment"
    "refsection"
    "verb"
    "sortlist"
    "entry")
   (LaTeX-add-counters
    "listtotal"
    "listcount"
    "liststart"
    "liststop"
    "citecount"
    "citetotal"
    "multicitecount"
    "multicitetotal"
    "instcount"
    "maxnames"
    "minnames"
    "maxitems"
    "minitems"
    "citecounter"
    "savedcitecounter"
    "uniquelist"
    "uniquename"
    "refsection"
    "refsegment"
    "maxextratitle"
    "maxextratitleyear"
    "maxextrayear"
    "maxextraalpha"
    "abbrvpenalty"
    "highnamepenalty"
    "lownamepenalty"
    "maxparens"
    "parenlevel"
    "mincomprange"
    "maxcomprange"
    "mincompwidth")
   (LaTeX-add-lengths
    "labelnumberwidth"
    "labelalphawidth"
    "shorthandwidth"
    "biblabelsep"
    "bibitemsep"
    "bibnamesep"
    "bibinitsep"
    "bibparsep"
    "bibhang")))

(defvar LaTeX-biblatex-package-options-list
  '(;;; Load-time Options
    ("backend" ("biber" "bibtex" "bibtexu" "bibtex8"))
    ("style" BibLaTeX-global-style-files)
    ("bibstyle" BibLaTeX-global-style-files)
    ("citestyle" BibLaTeX-global-style-files)
    ("natbib" ("true" "false"))
    ("mcite" ("true" "false"))
    ;;; Preamble Options
    ;; General
    ("sorting" ("nty" "nyt" "nyvt" "anyt" "anyvt" "ynt" "ydnt" "none" "debug"))
    ("sortcase" ("true" "false"))
    ("sortupper" ("true" "false"))
    ("sortlocale")
    ("sortlos" ("bib" "los"))
    ("related" ("true" "false"))
    ("sortcites" ("true" "false"))
    ("maxnames")
    ("minnames")
    ("maxbibnames")
    ("minbibnames")
    ("maxcitenames")
    ("mincitenames")
    ("maxitems")
    ("minitems")
    ("autocite" ("plain" "inline" "footnote" "superscript"))
    ("autopunct" ("true" "false"))
    ("language" ("auto" "catalan" "czech" "danish" "dutch" "american" "british"
		 "canadian" "australian" "newzealand" "finnish" "french"
		 "german" "austrian" "ngernam" "naustrian" "greek" "italian"
		 "norwegian" "brazilian" "portuguese" "russian" "spanish"
		 "swedish"))
    ("clearlang" ("true" "false"))
    ("babel" ("none" "hyphen" "other" "other*"))
    ("block" ("none" "space" "par" "nbpar" "ragged"))
    ("notetype" ("foot+end" "footonly" "endonly"))
    ("hyperref" ("true" "false"))
    ("backref" ("true" "false"))
    ("backrefstyle" ("none" "three" "two" "two+" "three+" "all+"))
    ("backrefsetstyle" ("setonly" "memonly" "setormem" "setandmem" "memandset" "setplusmem"))
    ("indexing" ("true" "false" "cite" "bib"))
    ("loadfiles" ("true" "false"))
    ("refsection" ("none" "part" "chapter" "section" "subsection"))
    ("refsegment" ("none" "part" "chapter" "section" "subsection"))
    ("citereset" ("none" "part" "chapter" "section" "subsection"))
    ("abbreviate" ("true" "false"))
    ("date" ("short" "long" "terse" "comp" "iso8601"))
    ("origdate" ("short" "long" "terse" "comp" "iso8601"))
    ("eventdate" ("short" "long" "terse" "comp" "iso8601"))
    ("urldate" ("short" "long" "terse" "comp" "iso8601"))
    ("alldates" ("short" "long" "terse" "comp" "iso8601"))
    ("datezeros" ("true" "false"))
    ("dateabbrev" ("true" "false"))
    ("defernumbers" ("true" "false"))
    ("punctfont" ("true" "false"))
    ("arxiv" ("abs" "ps" "pdf" "format"))
    ("texencoding" ("auto"))
    ("bibencoding" ("auto"))
    ("safeinputenc" ("true" "false"))
    ("bibwarn" ("true" "false"))
    ("mincrossrefs")
    ;; Style-specific
    ("isbn" ("true" "false"))
    ("url" ("true" "false"))
    ("doi" ("true" "false"))
    ("eprint" ("true" "false"))
    ;; Internal
    ("pagetracker" ("true" "false" "page" "spread"))
    ("citecounter" ("true" "false" "context"))
    ("citetracker" ("true" "false" "context" "strict" "constrict"))
    ("ibidtracker" ("true" "false" "context" "strict" "constrict"))
    ("opcittracker" ("true" "false" "context" "strict" "constrict"))
    ("loccittracker" ("true" "false" "context" "strict" "constrict"))
    ("idemtracker" ("true" "false" "context" "strict" "constrict"))
    ("parentracker" ("true" "false"))
    ("maxparens")
    ("firstinits" ("true" "false"))
    ("sortfirstinits" ("true" "false"))
    ("tersefirstinits" ("true" "false"))
    ("labelalpha" ("true" "false"))
    ("maxalphanames")
    ("minalphanames")
    ("labelnum" ("true" "false"))
    ("labeltitle" ("true" "false"))
    ("labeltitleyear" ("true" "false"))
    ("labelyear" ("true" "false"))
    ("singletitle" ("true" "false"))
    ("uniquename" ("true" "false" "init" "full" "allinit" "allfull" "mininit" "minfull"))
    ("uniquelist" ("true" "false" "minyear"))
    ;;; Entry Options
    ;; Preamble/Type/Entry Options
    ("useauthor" ("true" "false"))
    ("useeditor" ("true" "false"))
    ("usetranslator" ("true" "false"))
    ("useprefix" ("true" "false"))
    ("indexing" ("true" "false" "cite" "bib"))
    ;; Type/Entry Options are not available globally.
    ;; Legacy Options (deprecated)
    ("openbib"))
  "Package options for the biblatex package.")

(defun LaTeX-biblatex-package-options nil
  "Prompt for package options for the biblatex package."
  (unless BibLaTeX-global-style-files
    (if (eq TeX-arg-input-file-search t)  ;; Treat `ask' value as `nil'.
	;; ...then, search for BibLaTeX styles.
	(progn
	  (message "Searching for BibLaTeX styles...")
	  (setq BibLaTeX-global-style-files
		(mapcar 'identity (TeX-search-files-by-type 'bbxinputs 'global t t))))
      ;; ...else, use default BibLaTeX styles.
      (setq BibLaTeX-global-style-files
	    '("numeric" "numeric-comp" "numeric-verb" "alphabetic"
	      "alphabetic-verb" "authoryear" "authoryear-comp" "authoryear-ibid"
	      "authoryear-icomp" "authortitle" "authortitle-comp"
	      "authortitle-ibid" "authortitle-icomp" "authortitle-terse"
	      "authortitle-tcomp" "authortitle-ticomp" "verbose" "verbose-ibid"
	      "verbose-note" "verbose-inote" "verbose-trad1" "verbose-trad2"
	      "verbose-trad3" "reading" "draft" "debug"))))
  ;; Can't use directly `TeX-arg-key-val' because that would insert an empty
  ;; `[]' after `\usepackage' when `options' is empty.
  (let ((options (multi-prompt-key-value
		  (TeX-argument-prompt optional "Options (k=v)" nil)
		  LaTeX-biblatex-package-options-list)))
    options))

;;; biblatex.el ends here
