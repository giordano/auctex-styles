;;; siunitx.el --- AUCTeX style for `siunitx.sty' version 2.5p.

;; Copyright (C) 2012-2013 Free Software Foundation, Inc.

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

;; This file adds support for `siunitx.sty' version 2.5p.

;;; Code:

;; Self Parsing -- see (info "(auctex)Hacking the Parser").  `\\(?:\\[.*\\]\\)?'
;; matches possible options (actually used only by `DeclareSIUnit' macro),
;; wrapped in `[...]'.
(defvar LaTeX-siunitx-regexp
  '("\\\\Declare\\(?:SIUnit\\|SIPrefix\\|BinaryPrefix\\|SIPostPower\\|SIPrepower\\|SIQualifier\\)[ \t\n]*\\(?:\\[.*\\]\\)?[ \t\n]*{?\\\\\\([A-Za-z]+\\)}?" 1 LaTeX-auto-siunitx)
  "Matches new siunitx unit, prefix, power, and qualifier definitions.")

(defvar LaTeX-auto-siunitx nil
  "Temporary for parsing siunitx macro definitions.")

(defun LaTeX-siunitx-prepare ()
  "Clear `LaTex-auto-siunitx' before use."
  (setq LaTeX-auto-siunitx nil))

(defun LaTeX-siunitx-cleanup ()
  "Move symbols from `LaTeX-auto-siunitx' to `TeX-auto-symbol'."
  (mapcar (lambda (symbol)
	    ;; `siunitx' unit macros don't take arguments, we add them to
	    ;; `TeX-auto-symbol' in the form `("symbol" 0)'
	    (setq TeX-auto-symbol (cons (list symbol 0) TeX-auto-symbol)))
	  LaTeX-auto-siunitx))

;; FIXME: This does not seem to work unless one does a manual reparse.
(add-hook 'TeX-auto-prepare-hook 'LaTeX-siunitx-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-siunitx-cleanup)

(defvar LaTeX-siunitx-package-options
  '(;; Detecting fonts
    ("detect-all")
    ("detect-display-math" ("true" "false"))
    ("detect-family" ("true" "false"))
    ("detect-inline-family" ("math" "text"))
    ("detect-inline-weight" ("math" "text"))
    ("detect-mode" ("true" "false"))
    ("detect-none")
    ("detect-shape" ("true" "false"))
    ("detect-weight" ("true" "false"))
    ;; Font settings
    ("color")
    ("math-rm")
    ("math-sf")
    ("math-tt")
    ("mode" ("math" "text"))
    ("number-color")
    ("text-rm")
    ("text-sf")
    ("text-tt")
    ("unit-color")
    ;; Parsing numbers
    ("input-close-uncertainty")
    ("input-comparators")
    ("input-complex-roots")
    ("input-decimal-markers")
    ("input-digits")
    ("input-exponent-markers")
    ("input-ignore")
    ("input-open-uncertainty")
    ("input-protect-tokens")
    ("input-signs")
    ("input-uncertainty-signs")
    ("input-symbols")
    ("parse-numbers" ("true" "false"))
    ;; Post-processing numbers
    ("add-decimal-zero" ("true" "false"))
    ("add-integer-zero" ("true" "false"))
    ("explicit-sign")
    ("fixed-exponent")
    ("minimum-integer-digits")
    ("omit-uncertainty" ("true" "false"))
    ("retain-explicit-plus" ("true" "false"))
    ("retain-unity-mantissa" ("true" "false"))
    ("retain-zero-exponent" ("true" "false"))
    ("round-half" ("up" "even"))
    ("round-integer-to-decimal" ("true" "false"))
    ("round-minimum")
    ("round-mode" ("off" "figures" "places"))
    ("round-precision")
    ("scientific-notation" ("true" "false" "fixed" "engineering"))
    ("zero-decimal-to-integer" ("true" "false"))
    ;; Printing numbers
    ("bracket-negative-numbers" ("true" "false"))
    ("bracket-numbers" ("true" "false"))
    ("close-bracket")
    ("complex-root-position" ("after-number" "before-number"))
    ("copy-complex-root")
    ("copy-decimal-marker")
    ("exponent-base")
    ("exponent-product")
    ("group-digits" ("true" "false" "decimal" "integer"))
    ("group-minimum-digits")
    ("group-separator")
    ("negative-color")
    ("open-bracket")
    ("output-close-uncertainty")
    ("output-complex-root")
    ("output-decimal-marker")
    ("output-exponent-marker")
    ("output-open-uncertainty")
    ("separate-uncertainty" ("true" "false"))
    ("tight-spacing" ("true" "false"))
    ("uncertainty-separator")
    ;; Multi-part numbers
    ("fraction-function")
    ("input-product")
    ("input-quotient")
    ("output-product")
    ("output-quotient")
    ("quotient-mode" ("symbol" "fraction"))
    ;; Lists and ranges of numbers
    ("list-final-separator")
    ("list-pair-separator")
    ("list-separator")
    ("range-phrase")
    ;; Angles
    ("add-arc-degree-zero" ("true" "false"))
    ("add-arc-minute-zero" ("true" "false"))
    ("add-arc-second-zero" ("true" "false"))
    ("angle-symbol-over-decimal" ("true" "false"))
    ("arc-separator")
    ("number-angle-product")
    ;; Creating units
    ("free-standing-units" ("true" "false"))
    ("overwrite-functions" ("true" "false"))
    ("space-before-unit" ("true" "false"))
    ("unit-optional-argument" ("true" "false"))
    ("use-xspace" ("true" "false"))
    ;; Loading additional units
    ("abbreviations" ("true" "false"))
    ("binary-units" ("true" "false"))
    ("version-1-compatibility" ("true" "false"))
    ;; Using units
    ("bracket-unit-denominator" ("true" "false"))
    ("forbid-literal-units" ("true" "false"))
    ("literal-superscript-as-power" ("true" "false"))
    ("inter-unit-product")
    ("parse-units" ("true" "false"))
    ("per-mode" ("reciprocal" "fraction" "reciprocal-positive-first" "symbol" "repeated-symbol" "symbol-or-fraction"))
    ("per-symbol")
    ("power-font" ("number" "unit"))
    ("prefixes-as-symbols" ("true" "false"))
    ("qualifier-mode" ("subscript" "brackets" "phrase" "space" "text"))
    ("qualifier-phrase")
    ("sticky-per" ("true" "false"))
    ;; Numbers with units
    ("allow-number-unit-breaks" ("true" "false"))
    ("exponent-to-prefix" ("true" "false"))
    ("list-units" ("brackets" "repeat" "single"))
    ("multi-part-units" ("brackets" "repeat" "single"))
    ("number-unit-product")
    ("product-units" ("repeat" "brackets" "brackets-power" "power" "repeat" "single"))
    ("range-units" ("brackets" "repeat" "single"))
    ;; Tabular material
    ("table-align-comparator" ("true" "false"))
    ("table-align-exponent" ("true" "false"))
    ("table-align-text-pre" ("true" "false"))
    ("table-align-text-post" ("true" "false"))
    ("table-align-uncertainty" ("true" "false"))
    ("table-alignment" ("center" "left" "right"))
    ("table-auto-round" ("true" "false"))
    ("table-column-width")
    ("table-comparator" ("true" "false"))
    ("table-figures-decimal")
    ("table-figures-exponent")
    ("table-figures-integer")
    ("table-figures-uncertainty")
    ("table-format")
    ("table-number-alignment" ("center-decimal-marker" "center" "left" "right"))
    ("table-parse-only" ("true" "false"))
    ("table-omit-exponent" ("true" "false"))
    ("table-space-text-pre")
    ("table-space-text-post")
    ("table-sign-exponent" ("true" "false"))
    ("table-sign-mantissa" ("true" "false"))
    ("table-text-alignment" ("center" "left" "right"))
    ("table-unit-alignment" ("center" "left" "right"))
    ;; Symbols
    ("math-angstrom")
    ("math-arcminute")
    ("math-arcsecond")
    ("math-celsius")
    ("math-degree")
    ("math-micro")
    ("math-ohm")
    ("redefine-symbols" ("true" "false"))
    ("text-angstrom")
    ("text-arcminute")
    ("text-arcsecond")
    ("text-celsius")
    ("text-degree")
    ("text-micro")
    ("text-ohm")
    ;; Other options
    ("locale" ("FR" "DE" "UK" "US" "ZA"))
    ("strict"))
  "Package options for the siunitx package.")

(TeX-add-style-hook
 "siunitx"
 (lambda ()
   (TeX-auto-add-regexp LaTeX-siunitx-regexp)
   (TeX-add-symbols
    ;; Numbers
    '("ang" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Angle")
    '("num" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Number")
    '("numlist" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Numbers")
    '("numrange" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Number 1" "Number 2")
    ;; Units
    '("si" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Unit")
    '("SI" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Value" [ "Pre-unit"] "Unit")
    '("SIlist" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Values" "Unit")
    '("SIrange" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Value 1" "Value 2" "Unit")
    ;; Settings
    '("sisetup" (TeX-arg-key-val LaTeX-siunitx-package-options))
    ;; Tabular material
    '("tablenum" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] "Number")
    ;; Creating new macros (`DeclareSIUnitWithOptions' macro is deprecated)
    '("DeclareSIUnit" [ (TeX-arg-key-val LaTeX-siunitx-package-options) ] (TeX-arg-define-macro "Unit: \\") "Symbol")
    '("DeclareSIPrefix" (TeX-arg-define-macro "Prefix: \\") "Symbol" "Powers of 10")
    '("DeclareBinaryPrefix" (TeX-arg-define-macro "Prefix: \\") "Symbol" "Powers of 2")
    '("DeclareSIPostPower" (TeX-arg-define-macro "Name: \\") "Power")
    '("DeclareSIPrePower" (TeX-arg-define-macro "Name: \\") "Power")
    '("DeclareSIQualifier" (TeX-arg-define-macro "Qualifier: \\") "Symbol")
    ;;; The unit macros
    ;; SI base units
    '("ampere" 0)
    '("candela" 0)
    '("kelvin" 0)
    '("kilogram" 0)
    '("gram" 0)
    '("meter" 0)
    '("metre" 0)
    '("second" 0)
    ;; Coherent derived units in the SI with special names and symbols
    '("becquerel" 0)
    '("celsius" 0)
    '("degreeCelsius" 0)
    '("coulomb" 0)
    '("farad" 0)
    '("gray" 0)
    '("hertz" 0)
    '("henry" 0)
    '("joule" 0)
    '("katal" 0)
    '("lumen" 0)
    '("lux" 0)
    '("newton" 0)
    '("ohm" 0)
    '("pascal" 0)
    '("radians" 0)
    '("siemens" 0)
    '("sievert" 0)
    '("steradian" 0)
    '("tesla" 0)
    '("volt" 0)
    '("watt" 0)
    '("weber" 0)
    ;; Non-SI units accepted for use with the International System of Units
    '("day" 0)
    '("degree" 0)
    '("hectare" 0)
    '("hour" 0)
    '("liter" 0)
    '("litre" 0)
    '("arcminute" 0)
    '("minute" 0)
    '("arcsecond" 0)
    '("tonne" 0)
    ;; Non-SI units whose values in SI units must be obtained experimentally
    '("astronomicalunit" 0)
    '("atomicmassunit" 0)
    '("bohr" 0)
    '("clight" 0)
    '("dalton" 0)
    '("electronmass" 0)
    '("electronvolt" 0)
    '("elementarycharge" 0)
    '("hartree" 0)
    '("planckbar" 0)
    ;; Other non-SI units.
    '("angstrom" 0)
    '("bar" 0)
    '("barn" 0)
    '("bel" 0)
    '("decibel" 0)
    '("knot" 0)
    '("mmHg" 0)
    '("nauticalmile" 0)
    '("neper" 0)
    '("percent" 0)
    ;; SI prefixes
    '("yocto" 0)
    '("zepto" 0)
    '("atto" 0)
    '("femto" 0)
    '("pico" 0)
    '("nano" 0)
    '("micro" 0)
    '("milli" 0)
    '("centi" 0)
    '("deci" 0)
    '("deca" 0)
    '("deka" 0)
    '("hecto" 0)
    '("kilo" 0)
    '("mega" 0)
    '("giga" 0)
    '("tera" 0)
    '("peta" 0)
    '("exa" 0)
    '("zetta" 0)
    '("yotta" 0)
    ;; Powers
    '("square" 0)
    '("squared" 0)
    '("cubic" 0)
    '("cubed" 0)
    '("tothe" 0)
    '("raiseto" 0)
    '("per" 0)
    '("of" 0)
    ;; Highlighting
    '("highlight" "Color")
    ;; Abbreviated units (available unless `abbreviations' option is set to `false')
    '("fg" 0)
    '("pg" 0)
    '("ng" 0)
    '("ug" 0)
    '("mg" 0)
    '("g" 0)
    '("kg" 0)
    '("amu" 0)
    '("pm" 0)
    '("nm" 0)
    '("um" 0)
    '("mm" 0)
    '("cm" 0)
    '("dm" 0)
    '("m" 0)
    '("km" 0)
    '("as" 0)
    '("fs" 0)
    '("ps" 0)
    '("ns" 0)
    '("us" 0)
    '("ms" 0)
    '("s" 0)
    '("fmol" 0)
    '("pmol" 0)
    '("nmol" 0)
    '("umol" 0)
    '("mmol" 0)
    '("mol" 0)
    '("kmol" 0)
    '("pA" 0)
    '("nA" 0)
    '("uA" 0)
    '("mA" 0)
    '("A" 0)
    '("kA" 0)
    '("ul" 0)
    '("ml" 0)
    '("l" 0)
    '("hl" 0)
    '("uL" 0)
    '("mL" 0)
    '("L" 0)
    '("hL" 0)
    '("mHz" 0)
    '("Hz" 0)
    '("kHz" 0)
    '("MHz" 0)
    '("GHz" 0)
    '("THz" 0)
    '("N" 0)
    '("mN" 0)
    '("kN" 0)
    '("MN" 0)
    '("Pa" 0)
    '("kPa" 0)
    '("MPa" 0)
    '("GPa" 0)
    '("mohm" 0)
    '("kohm" 0)
    '("Mohm" 0)
    '("pV" 0)
    '("nV" 0)
    '("uV" 0)
    '("mV" 0)
    '("V" 0)
    '("kV" 0)
    '("uW" 0)
    '("mW" 0)
    '("W" 0)
    '("kW" 0)
    '("MW" 0)
    '("GW" 0)
    '("J" 0)
    '("kJ" 0)
    '("meV" 0)
    '("keV" 0)
    '("MeV" 0)
    '("GeV" 0)
    '("TeV" 0)
    '("kWh" 0)
    '("F" 0)
    '("fF" 0)
    '("pF" 0)
    '("K" 0)
    '("dB" 0)
    ;; Binary prefixes and units available when `binary-units' option is used
    '("kibi" 0)
    '("mebi" 0)
    '("gibi" 0)
    '("tebi" 0)
    '("pebi" 0)
    '("exbi" 0)
    '("zebi" 0)
    '("yobi" 0)
    '("bit" 0)
    '("byte" 0)
    ;; Symbols
    '("SIUnitSymbolAngstrom" 0)
    '("SIUnitSymbolArcminute" 0)
    '("SIUnitSymbolArcsecond" 0)
    '("SIUnitSymbolCelsius" 0)
    '("SIUnitSymbolDegree" 0)
    '("SIUnitSymbolMicro" 0)
    '("SIUnitSymbolOhm" 0)
    ;; Macros available when `version-1-compatibility' option is used
    '("Square" 0)
    '("ssquare" 0)
    '("BAR" 0)
    '("bbar" 0)
    '("Day" 0)
    '("dday" 0)
    '("Gray" 0)
    '("ggray" 0)
    '("atomicmass" 0)
    '("arcmin" 0)
    '("arcsec" 0)
    '("are" 0)
    '("curie" 0)
    '("gal" 0)
    '("millibar" 0)
    '("rad" 0)
    '("rem" 0)
    '("roentgen" 0)
    '("micA" 0)
    '("micmol" 0)
    '("micl" 0)
    '("micL" 0)
    '("nanog" 0)
    '("micg" 0)
    '("picm" 0)
    '("micm" 0)
    '("Sec" 0)
    '("mics" 0)
    '("cmc" 0)
    '("dmc" 0)
    '("cms" 0)
    '("centimetrecubed" 0)
    '("centimetresquared" 0)
    '("cubiccentimetre" 0)
    '("cubicdecimetre" 0)
    '("squarecentimetre" 0)
    '("squaremetre" 0)
    '("squarekilometre" 0)
    '("parsec" 0)
    '("lightyear" 0)
    '("gmol" 0)
    '("kgmol" 0)
    '("lbmol" 0)
    '("molar" 0)
    '("Molar" 0)
    '("torr" 0)
    '("gon" 0)
    '("micron" 0)
    '("mrad" 0)
    '("gauss" 0)
    '("eVperc" 0)
    '("nanobarn" 0)
    '("picobarn" 0)
    '("femtobarn" 0)
    '("attobarn" 0)
    '("zeptobarn" 0)
    '("yoctobarn" 0)
    '("nb" 0)
    '("pb" 0)
    '("fb" 0)
    '("ab" 0)
    '("zb" 0)
    '("yb" 0)
    ;; Transferring settings to pgf
    '("SendSettingsToPgf" 0))
   (TeX-run-style-hooks "l3keys2e"
			"array"
			"amstext"
			"xparse"
			"expl3")
   ;; Fontification
   (when (and (featurep 'font-latex)
   	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("ang" "[{")
   				("num" "[{")
				("si" "[{")
   				("SI" "[{[{")
   				("numlist" "[{")
   				("numrange" "[{{")
   				("SIlist" "[{{")
   				("SIrange" "[{{{")
   				("sisetup" "{")
   				("tablenum" "[{")
				("DeclareSIUnit" "[|{\\{")
				("DeclareSIPrefix" "|{\\{{")
				("DeclareBinaryPrefix" "|{\\{{")
				("DeclareSIPostPower" "|{\\{")
				("DeclareSIPrePower" "|{\\{")
				("DeclareSIQualifier" "|{\\{")
				("highlight" "{"))
   			      'function))))

;; siunitx.el ends here
