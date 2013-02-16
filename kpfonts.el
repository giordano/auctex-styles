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

;;; Kpfonts Minor Mode (heavily based on code of LaTeX Math Minor Mode)

(defconst LaTeX-kpfonts-default
  '(;; Other Greek Lowercase
    ("o a" "otheralpha" "Other Greek Lowercase" 945)
    ("o b" "otherbeta" "Other Greek Lowercase" 946)
    ("o g" "othergamma" "Other Greek Lowercase" 947)
    ("o d" "otherdelta" "Other Greek Lowercase" 948)
    ("o e" "otherepsilon" "Other Greek Lowercase" 1013)
    ("o z" "otherzeta" "Other Greek Lowercase" 950)
    ("o h" "othereta" "Other Greek Lowercase" 951)
    ("o j" "othertheta" "Other Greek Lowercase" 952)
    (nil "otheriota" "Other Greek Lowercase" 953)
    ("o k" "otherkappa" "Other Greek Lowercase" 954)
    ("o l" "otherlambda" "Other Greek Lowercase" 955)
    ("o m" "othermu" "Other Greek Lowercase" 956)
    ("o n" "othernu" "Other Greek Lowercase" 957)
    ("o x" "otherxi" "Other Greek Lowercase" 958)
    ("o p" "otherpi" "Other Greek Lowercase" 960)
    ("o r" "otherrho" "Other Greek Lowercase" 961)
    ("o s" "othersigma" "Other Greek Lowercase" 963)
    ("o t" "othertau" "Other Greek Lowercase" 964)
    ("o u" "otherupsilon" "Other Greek Lowercase" 965)
    ("o f" "otherphi" "Other Greek Lowercase" 981)
    ("o q" "otherchi" "Other Greek Lowercase" 967)
    ("o y" "otherpsi" "Other Greek Lowercase" 968)
    ("o w" "otheromega" "Other Greek Lowercase" 969)
    ("o v e" "othervarepsilon" "Other Greek Lowercase" 949)
    ("o v j" "othervartheta" "Other Greek Lowercase" 977)
    ("o v p" "othervarpi" "Other Greek Lowercase" 982)
    ("o v r" "othervarrho" "Other Greek Lowercase" 1009)
    ("o v s" "othervarsigma" "Other Greek Lowercase" 962)
    ("o v f" "othervarphi" "Other Greek Lowercase" 966)
    ;; Slanted Greek Lowercase
    (nil "alphasl" "Slanted Greek Lowercase" 120572)
    (nil "betasl" "Slanted Greek Lowercase" 120573)
    (nil "gammasl" "Slanted Greek Lowercase" 120574)
    (nil "deltasl" "Slanted Greek Lowercase" 120575)
    (nil "epsilonsl" "Slanted Greek Lowercase" 120598)
    (nil "zetasl" "Slanted Greek Lowercase" 120577)
    (nil "etasl" "Slanted Greek Lowercase" 120578)
    (nil "thetasl" "Slanted Greek Lowercase" 120579)
    (nil "iotasl" "Slanted Greek Lowercase" 120580)
    (nil "kappasl" "Slanted Greek Lowercase" 120581)
    (nil "lambdasl" "Slanted Greek Lowercase" 120582)
    (nil "musl" "Slanted Greek Lowercase" 120583)
    (nil "nusl" "Slanted Greek Lowercase" 120584)
    (nil "xisl" "Slanted Greek Lowercase" 120585)
    (nil "pisl" "Slanted Greek Lowercase" 120587)
    (nil "rhosl" "Slanted Greek Lowercase" 120588)
    (nil "sigmasl" "Slanted Greek Lowercase" 120590)
    (nil "tausl" "Slanted Greek Lowercase" 120591)
    (nil "upsilonsl" "Slanted Greek Lowercase" 120592)
    (nil "phisl" "Slanted Greek Lowercase" 120601)
    (nil "chisl" "Slanted Greek Lowercase" 120594)
    (nil "psisl" "Slanted Greek Lowercase" 120595)
    (nil "omegasl" "Slanted Greek Lowercase" 120596)
    (nil "varepsilonsl" "Slanted Greek Lowercase" 120576)
    (nil "varthetasl" "Slanted Greek Lowercase" 120599)
    (nil "varpisl" "Slanted Greek Lowercase" 120603)
    (nil "varrhosl" "Slanted Greek Lowercase" 120602)
    (nil "varsigmasl" "Slanted Greek Lowercase" 120589)
    (nil "varphisl" "Slanted Greek Lowercase" 120593)
    ;; Upright Greek Lowercase
    (nil "alphaup" "Upright Greek Lowercase" 945)
    (nil "betaup" "Upright Greek Lowercase" 946)
    (nil "gammaup" "Upright Greek Lowercase" 947)
    (nil "deltaup" "Upright Greek Lowercase" 948)
    (nil "epsilonup" "Upright Greek Lowercase" 1013)
    (nil "zetaup" "Upright Greek Lowercase" 950)
    (nil "etaup" "Upright Greek Lowercase" 951)
    (nil "thetaup" "Upright Greek Lowercase" 952)
    (nil "iotaup" "Upright Greek Lowercase" 953)
    (nil "kappaup" "Upright Greek Lowercase" 954)
    (nil "lambdaup" "Upright Greek Lowercase" 955)
    (nil "muup" "Upright Greek Lowercase" 956)
    (nil "nuup" "Upright Greek Lowercase" 957)
    (nil "xiup" "Upright Greek Lowercase" 958)
    (nil "piup" "Upright Greek Lowercase" 960)
    (nil "rhoup" "Upright Greek Lowercase" 961)
    (nil "sigmaup" "Upright Greek Lowercase" 963)
    (nil "tauup" "Upright Greek Lowercase" 964)
    (nil "upsilonup" "Upright Greek Lowercase" 965)
    (nil "phiup" "Upright Greek Lowercase" 981)
    (nil "chiup" "Upright Greek Lowercase" 967)
    (nil "psiup" "Upright Greek Lowercase" 968)
    (nil "omegaup" "Upright Greek Lowercase" 969)
    (nil "varepsilonup" "Upright Greek Lowercase" 949)
    (nil "varthetaup" "Upright Greek Lowercase" 977)
    (nil "varpiup" "Upright Greek Lowercase" 982)
    (nil "varrhoup" "Upright Greek Lowercase" 1009)
    (nil "varsigmaup" "Upright Greek Lowercase" 962)
    (nil "varphiup" "Upright Greek Lowercase" 966)
    ;; Other Greek Uppercase
    ("o G" "otherGamma" "Other Greek Uppercase" 915)
    ("o D" "otherDelta" "Other Greek Uppercase" 916)
    ("o J" "otherTheta" "Other Greek Uppercase" 920)
    ("o L" "otherLambda" "Other Greek Uppercase" 923)
    ("o X" "otherXi" "Other Greek Uppercase" 926)
    ("o P" "otherPi" "Other Greek Uppercase" 928)
    ("o S" "otherSigma" "Other Greek Uppercase" 931)
    ("o U" "otherUpsilon" "Other Greek Uppercase" 978)
    ("o F" "otherPhi" "Other Greek Uppercase" 934)
    ("o Y" "otherPsi" "Other Greek Uppercase" 936)
    ("o W" "otherOmega" "Other Greek Uppercase" 937)
    ;; Slanted Greek Uppercase
    (nil "Gammasl" "Slanted Greek Uppercase" 120548)
    (nil "Deltasl" "Slanted Greek Uppercase" 120549)
    (nil "Thetasl" "Slanted Greek Uppercase" 120553)
    (nil "Lambdasl" "Slanted Greek Uppercase" 120556)
    (nil "Xisl" "Slanted Greek Uppercase" 120559)
    (nil "Pisl" "Slanted Greek Uppercase" 120561)
    (nil "Sigmasl" "Slanted Greek Uppercase" 120564)
    (nil "Upsilonsl" "Slanted Greek Uppercase" 120566)
    (nil "Phisl" "Slanted Greek Uppercase" 120567)
    (nil "Psisl" "Slanted Greek Uppercase" 120569)
    (nil "Omegasl" "Slanted Greek Uppercase" 120570)
    ;; Upright Greek Uppercase
    (nil "Gammaup" "Upright Greek Uppercase" 915)
    (nil "Deltaup" "Upright Greek Uppercase" 916)
    (nil "Thetaup" "Upright Greek Uppercase" 920)
    (nil "Lambdaup" "Upright Greek Uppercase" 923)
    (nil "Xiup" "Upright Greek Uppercase" 926)
    (nil "Piup" "Upright Greek Uppercase" 928)
    (nil "Sigmaup" "Upright Greek Uppercase" 931)
    (nil "Upsilonup" "Upright Greek Uppercase" 978)
    (nil "Phiup" "Upright Greek Uppercase" 934)
    (nil "Psiup" "Upright Greek Uppercase" 936)
    (nil "Omegaup" "Upright Greek Uppercase" 937)
    ;; Integrals
    (nil "varint" "Integrals" nil)
    (nil "variint" "Integrals" nil)
    (nil "variiint" "Integrals" nil)
    (nil "variiiint" "Integrals" nil)
    (nil "varidotsint" "Integrals" nil)
    (nil "oiint" "Integrals" 8751)
    (nil "ointctrclockwise" "Integrals" 8755)
    (nil "ointclockwise" "Integrals" nil)
    (nil "sqint" "Integrals" 10774)
    (nil "idotsint" "Integrals" nil)
    (nil "oiiint" "Integrals" 8752)
    (nil "varointctrclockwise" "Integrals" nil)
    (nil "varointclockwise" "Integrals" 8754)
    (nil "fint" "Integrals" 10767)
    (nil "oiintctrclockwise" "Integrals" nil)
    (nil "varoiintclockwise" "Integrals" nil)
    (nil "oiiintctrclockwise" "Integrals" nil)
    (nil "varoiiintclockwise" "Integrals" nil)
    (nil "oiintclockwise" "Integrals" nil)
    (nil "varoiintctrclockwise" "Integrals" nil)
    (nil "oiiintclockwise" "Integrals" nil)
    (nil "varoiiintctrclockwise" "Integrals" nil)
    (nil "sqiint" "Integrals" nil)
    (nil "sqiiint" "Integrals" nil)
    ;; Mapping
    (nil "mappedfrom" "Mapping" 8612)
    (nil "longmappedfrom" "Mapping" 10235)
    (nil "Mapsto" "Mapping" 10503)
    (nil "Longmapsto" "Mapping" 10238)
    (nil "Mappedfrom" "Mapping" 10502)
    (nil "Longmappedfrom" "Mapping" 10237)
    (nil "mmapsto" "Mapping" nil)
    (nil "longmmapsto" "Mapping" nil)
    (nil "mmappedfrom" "Mapping" nil)
    (nil "longmmappedfrom" "Mapping" nil)
    (nil "Mmapsto" "Mapping" nil)
    (nil "Longmmapsto" "Mapping" nil)
    (nil "Mmappedfrom" "Mapping" nil)
    (nil "Longmmappedfrom" "Mapping" nil)
    ;; Arrows
    (nil "dashleftarrow" "Arrows" 10510)
    (nil "dashrightarrow" "Arrows" 10511)
    (nil "dashleftrightarrow" "Arrows" nil)
    (nil "leftsquigarrow" "Arrows" 8668)
    (nil "Nearrow" "Arrows" 8663)
    (nil "Searrow" "Arrows" 8664)
    (nil "Nwarrow" "Arrows" 8662)
    (nil "Swarrow" "Arrows" 8665)
    (nil "leadstoext" "Arrows" 12316)
    (nil "leadsto" "Arrows" 10547)
    (nil "boxright" "Arrows" nil)
    (nil "Diamondright" "Arrows" nil)
    (nil "circleright" "Arrows" nil)
    (nil "boxleft" "Arrows" nil)
    (nil "Diamondleft" "Arrows" nil)
    (nil "circleleft" "Arrows" nil)
    (nil "boxdotright" "Arrows" nil)
    (nil "Diamonddotright" "Arrows" nil)
    (nil "circledotright" "Arrows" nil)
    (nil "boxdotleft" "Arrows" nil)
    (nil "Diamonddotleft" "Arrows" nil)
    (nil "circledotleft" "Arrows" nil)
    (nil "boxRight" "Arrows" nil)
    (nil "boxLeft" "Arrows" nil)
    (nil "boxdotRight" "Arrows" nil)
    (nil "boxdotLeft" "Arrows" nil)
    (nil "DiamondRight" "Arrows" nil)
    (nil "DiamondLeft" "Arrows" nil)
    (nil "DiamonddotRight" "Arrows" nil)
    (nil "DiamonddotLeft" "Arrows" nil)
    ;; Neg Arrows
    (nil "ntwoheadrightarrow" "Neg Arrows" 10496)
    (nil "ntwoheadleftarrow" "Neg Arrows" 11060)
    ;; Binary Op
    (nil "multimap" "Binary Op" 8888)
    (nil "multimapinv" "Binary Op" 10204)
    (nil "multimapboth" "Binary Op" 10719)
    (nil "multimapdot" "Binary Op" nil)
    (nil "multimapdotinv" "Binary Op" nil)
    (nil "multimapdotboth" "Binary Op" nil)
    (nil "multimapdotbothA" "Binary Op" 8886)
    (nil "multimapdotbothB" "Binary Op" 8887)
    (nil "multimapbothvert" "Binary Op" nil)
    (nil "multimapdotbothvert" "Binary Op" nil)
    (nil "multimapdotbothAvert" "Binary Op" nil)
    (nil "multimapdotbothBvert" "Binary Op" nil)
    (nil "Wr" "Binary Op" nil)
    (nil "sqcupplus" "Binary Op" nil)
    (nil "sqcapplus" "Binary Op" nil)
    (nil "medcirc" "Binary Op" 9898)
    (nil "medbullet" "Binary Op" 9899)
    (nil "invamp" "Binary Op" 8523)
    (nil "Diamonddot" "Binary Op" 10192)
    (nil "Diamond" "Binary Op" 9671)
    (nil "Diamondblack" "Binary Op" 9670)
    (nil "strictif" "Binary Op" 8880)
    (nil "strictfi" "Binary Op" 8881)
    (nil "strictiff" "Binary Op" nil)
    (nil "circledless" "Binary Op" 10688)
    (nil "circledgtr" "Binary Op" 10689)
    (nil "circledwedge" "Binary Op" nil)
    (nil "circledvee" "Binary Op" nil)
    (nil "circledbar" "Binary Op" 10678)
    (nil "circledbslash" "Binary Op" 10680)
    (nil "bignplus" "Binary Op" nil)
    (nil "bigsqcupplus" "Binary Op" nil)
    (nil "bigsqcapplus" "Binary Op" nil)
    (nil "bigsqcap" "Binary Op" 10757)
    (nil "varprod" "Binary Op" 10761)
    ;; Relational
    (nil "doteq" "Relational" 8784)
    (nil "VDash" "Relational" 8875)
    (nil "VvDash" "Relational" nil)
    (nil "cong" "Relational" 8773)
    (nil "preceqq" "Relational" 10931)
    (nil "succeqq" "Relational" 10932)
    (nil "coloneqq" "Relational" nil)
    (nil "varparallel" "Relational" 11005)
    (nil "nvarparallel" "Relational" nil)
    (nil "varparallelinv" "Relational" nil)
    (nil "nvarparallelinv" "Relational" nil)
    (nil "colonapprox" "Relational" nil)
    (nil "colonsim" "Relational" nil)
    (nil "Colonapprox" "Relational" nil)
    (nil "Colonsim" "Relational" nil)
    (nil "eqqcolon" "Relational" 8789)
    (nil "coloneq" "Relational" nil)
    (nil "eqcolon" "Relational" 8761)
    (nil "Coloneqq" "Relational" 10868)
    (nil "Eqqcolon" "Relational" nil)
    ;; Neg Rel
    (nil "nprecsim" "Neg Rel" nil)
    (nil "nsuccsim" "Neg Rel" nil)
    (nil "nlesssim" "Neg Rel" 8820)
    (nil "ngtrsim" "Neg Rel" 8821)
    (nil "nlessapprox" "Neg Rel" nil)
    (nil "ngtrapprox" "Neg Rel" nil)
    (nil "npreccurlyeq" "Neg Rel" 8928)
    (nil "nsucccurlyeq" "Neg Rel" 8929)
    (nil "ngtrless" "Neg Rel" 8825)
    (nil "nlessgtr" "Neg Rel" 8824)
    (nil "nbumpeq" "Neg Rel" nil)
    (nil "nBumpeq" "Neg Rel" nil)
    (nil "nbacksim" "Neg Rel" nil)
    (nil "nbacksimeq" "Neg Rel" nil)
    (nil "nasymp" "Neg Rel" 8813)
    (nil "nequiv" "Neg Rel" 8802)
    (nil "nsim" "Neg Rel" 8769)
    (nil "napprox" "Neg Rel" 8777)
    (nil "nsubset" "Neg Rel" 8836)
    (nil "nsupset" "Neg Rel" 8837)
    (nil "nll" "Neg Rel" nil)
    (nil "ngg" "Neg Rel" nil)
    (nil "nthickapprox" "Neg Rel" 8777)
    (nil "napproxeq" "Neg Rel" nil)
    (nil "nprecapprox" "Neg Rel" nil)
    (nil "nsuccapprox" "Neg Rel" nil)
    (nil "npreceqq" "Neg Rel" nil)
    (nil "nsucceqq" "Neg Rel" nil)
    (nil "nsimeq" "Neg Rel" 8772)
    (nil "notin" "Neg Rel" 8713)
    (nil "notni" "Neg Rel" 8716)
    (nil "nSubset" "Neg Rel" nil)
    (nil "nSupset" "Neg Rel" nil)
    (nil "nsqsubseteq" "Neg Rel" 8930)
    (nil "nsqsupseteq" "Neg Rel" 8931)
    (nil "nsqsubset" "Neg Rel" nil)
    (nil "nsqsupset" "Neg Rel" nil)
    ;; Delimeters
    (nil "Lbag" "Delimeters" 10181)
    (nil "Rbag" "Delimeters" 10182)
    (nil "llbracket" "Delimeters" 10214)
    (nil "rrbracket" "Delimeters" 10215)
    ;; Accents
    (nil "widearc" "Accents" 8978)
    (nil "widearcarrow" "Accents" 8405)
    (nil "wideOarc" "Accents" 8405)
    (nil "wideparen" "Accents" 9180)
    (nil "widering" "Accents" nil)
    ;; Misc
    ("v 0" "varemptyset" "Misc" 8709)
    (nil "lJoin" "Misc" 8905)
    (nil "rJoin" "Misc" 8906)
    (nil "Join" "Misc" 8904)
    (nil "openJoin" "Misc" nil)
    (nil "lrtimes" "Misc" nil)
    (nil "opentimes" "Misc" nil)
    (nil "nplus" "Misc" nil)
    (nil "Top" "Misc" 10986)
    (nil "Bot" "Misc" 10987)
    (nil "Perp" "Misc" 10987)
    (nil "boxast" "Misc" nil)
    (nil "boxbslash" "Misc" nil)
    (nil "boxbar" "Misc" nil)
    (nil "boxslash" "Misc" nil)
    (nil "lambdaslash" "Misc" 411)
    (nil "lambdabar" "Misc" 411)
    (nil "varclubsuit" "Misc" 9831)
    (nil "vardiamondsuit" "Misc" 9830)
    (nil "varheartsuit" "Misc" 9829)
    (nil "varspadesuit" "Misc" 9828))
  "Alist of kpfonts symbols.

Each entry should be a list with upto four elements, KEY, VALUE,
MENU and CHARACTER.

KEY is the key (after `LaTeX-kpfonts-abbrev-prefix') to be
redefined in kpfonts minor mode.  If KEY is nil, the symbol has
no associated keystroke \(it is available in the menu, though\).

VALUE can be a string with the name of the macro to be inserted,
or a function to be called.  The macro must be given without the
leading backslash.

The third element MENU is the name of the submenu where the
command should be added.  MENU can be either a string
\(e.g. \"greek\"\), a list (e.g. \(\"AMS\" \"Delimiters\"\)\) or
nil.  If MENU is nil, no menu item will be created.

The fourth element CHARACTER is a Unicode character position for
menu display.  When nil, no character is shown.

See also `LaTeX-kpfonts-menu'.")

(defvar LaTeX-kpfonts-abbrev-prefix LaTeX-math-abbrev-prefix
  "Prefix key for use in `LaTeX-kpfonts-mode'.
This has to be a string representing a key sequence in a format
understood by the `kbd' macro.  This corresponds to the syntax
usually used in the Emacs and Elisp manuals.")

(defun LaTeX-kpfonts-abbrev-prefix ()
  "Make a key definition from the variable `LaTeX-kpfonts-abbrev-prefix'."
  (if (stringp LaTeX-kpfonts-abbrev-prefix)
      (read-kbd-macro LaTeX-kpfonts-abbrev-prefix)
    LaTeX-kpfonts-abbrev-prefix))

(defvar LaTeX-kpfonts-keymap (make-sparse-keymap)
  "Keymap used for `LaTeX-kpfonts-mode' commands.")

(defvar LaTeX-kpfonts-menu nil
  "Menu containing commands provided by kpfonts LaTeX package.
The menu entries will be generated dynamically, but you can specify
the sequence by initializing this variable.")

;; We set `LaTeX-kpfonts-menu' after its definition because otherwise resetting
;; AUCTeX with `C-u C-c C-n' would create duplicate entries in menu.
(setq LaTeX-kpfonts-menu
      '("Kpfonts"
	("Insert Font"
	 ["Math Upright"             (TeX-font nil ?\C-h) :keys "C-c C-f C-h"]
	 ["Math Fraktur"             (TeX-font nil ?\C-k) :keys "C-c C-f C-k"]
	 ["Math Script"              (TeX-font nil ?\C-p) :keys "C-c C-f C-p"]
	 ["Slanted Small Caps"       (TeX-font nil ?\C-l) :keys "C-c C-f C-l"]
	 ["Other Small Caps"         (TeX-font nil ?\C-o) :keys "C-c C-f C-o"]
	 ["Other Slanted Small Caps" (TeX-font nil ?\C-q) :keys "C-c C-f C-q"])
	("Replace Font"
	 ["Math Upright"             (TeX-font t ?\C-h) :keys "C-u C-c C-f C-h"]
	 ["Math Fraktur"             (TeX-font t ?\C-k) :keys "C-u C-c C-f C-k"]
	 ["Math Script"              (TeX-font t ?\C-p) :keys "C-u C-c C-f C-p"]
	 ["Slanted Small Caps"       (TeX-font t ?\C-l) :keys "C-u C-c C-f C-l"]
	 ["Other Small Caps"         (TeX-font t ?\C-o) :keys "C-u C-c C-f C-o"]
	 ["Other Slanted Small Caps" (TeX-font t ?\C-q) :keys "C-u C-c C-f C-q"])
	["Delete Font"              (TeX-font t ?\C-d) :keys "C-c C-f C-d"]
	"-"
	("Other Greek Lowercase") ("Slanted Greek Lowercase")
	("Upright Greek Lowercase") ("Other Greek Uppercase")
	("Slanted Greek Uppercase") ("Upright Greek Uppercase") ("Integrals")
	("Mapping") ("Arrows") ("Neg Arrows") ("Binary Op") ("Relational")
	("Neg Rel") ("Delimeters") ("Accents") ("Misc")))

(let ((math (reverse LaTeX-kpfonts-default))
      (map LaTeX-kpfonts-keymap)
      (unicode (and LaTeX-math-menu-unicode (fboundp 'decode-char))))
  (while math
    (let* ((entry (car math))
	   (key (nth 0 entry))
	   (prefix
	    (and unicode
		 (nth 3 entry)))
	   value menu name)
      (setq math (cdr math))
      (if (and prefix
	       (setq prefix (decode-char 'ucs (nth 3 entry))))
	  (setq prefix (concat (string prefix) " \\"))
	(setq prefix "\\"))
      (if (listp (cdr entry))
	  (setq value (nth 1 entry)
		menu (nth 2 entry))
	(setq value (cdr entry)
	      menu nil))
      (if (stringp value)
	  (progn
	    (setq name (intern (concat "LaTeX-kpfonts-" value)))
	    (fset name (list 'lambda (list 'arg) (list 'interactive "*P")
			     (list 'LaTeX-math-insert value 'arg))))
	(setq name value))
      (if key
	  (progn
	    (setq key (cond ((numberp key) (char-to-string key))
			    ((stringp key) (read-kbd-macro key))
			    (t (vector key))))
	    (define-key map key name)))
      (if menu
	  (let ((parent LaTeX-kpfonts-menu))
	    (if (listp menu)
		(progn
		  (while (cdr menu)
		    (let ((sub (assoc (car menu) LaTeX-kpfonts-menu)))
		      (if sub
			  (setq parent sub)
			(setcdr parent (cons (list (car menu)) (cdr parent))))
		      (setq menu (cdr menu))))
		  (setq menu (car menu))))
	    (let ((sub (assoc menu parent)))
	      (if sub
		  (if (stringp value)
		      (setcdr sub (cons (vector (concat prefix value)
						name t)
					(cdr sub)))
		    (error "Cannot have multiple special kpfonts menu items"))
		(setcdr parent
			(cons (if (stringp value)
				  (list menu (vector (concat prefix value)
						     name t))
				(vector menu name t))
			      (cdr parent)))))))))
  ;; Make the kpfonts prefix char available if it has not been used as a prefix.
  (unless (lookup-key map (LaTeX-kpfonts-abbrev-prefix))
    (define-key map (LaTeX-kpfonts-abbrev-prefix) 'self-insert-command)))

(define-minor-mode LaTeX-kpfonts-mode
  "A minor mode with easy access to kpfonts macros.

Easy insertion of kpfonts symbols.  If you give a prefix
argument, the symbols will be surrounded by dollar signs.  The
following commands are defined:

\\{LaTeX-kpfonts-mode-map}"
  nil nil (list (cons (LaTeX-kpfonts-abbrev-prefix) LaTeX-kpfonts-keymap))
  (if LaTeX-kpfonts-mode
      (easy-menu-add LaTeX-kpfonts-mode-menu LaTeX-kpfonts-mode-map)
    (easy-menu-remove LaTeX-kpfonts-mode-menu))
  (TeX-set-mode-name))

(easy-menu-define LaTeX-kpfonts-mode-menu
  LaTeX-kpfonts-mode-map
  "Menu used in kpfonts minor mode."
  LaTeX-kpfonts-menu)

(defvar LaTeX-kpfonts-mode-enable LaTeX-math-mode
  "Whether to enable kpfonts minor mode.")

(if LaTeX-kpfonts-mode-enable
    (LaTeX-kpfonts-mode))
;;; Kpfonts Minor Mode ends here

;; New fonts by `kpfonts'.
(setq TeX-font-list
      (append
       TeX-font-list
       '(;; Math fonts
	 (?\C-h "" "" "\\mathup{"   "}")
	 (?\C-k "" "" "\\mathfrak{" "}")
	 (?\C-p "" "" "\\mathscr{"  "}")
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
   (TeX-add-symbols
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
    ;; New extensive symbols
    '("widearc" 1)
    '("widearcarrow" 1)
    '("wideOarc" 1)
    '("wideparen" 1)
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
