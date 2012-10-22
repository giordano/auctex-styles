;;; siunitx.el --- AUCTeX style for `siunitx.sty'

;; Author: Mosè Giordano <giordano.mose@libero.it>
;; Created: 2012-10-20
;; Keywords: tex

;;; Commentary:

;; This file adds support for `siunitx.sty'.

;;; Code:

(TeX-add-style-hook "siunitx"
 (lambda ()
   (TeX-add-symbols
    '("ang" [ "Options" ] "Angle")
    '("num" [ "Options" ] "Number")
    '("si" [ "Options" ] "Unit")
    '("SI" [ "Options" ] "Value" [ "Pre-unit"] "Unit")
    '("numlist" [ "Options" ] "Numbers")
    '("numrange" [ "Options" ] "Number 1" "Number 2")
    '("SIlist" [ "Options" ] "Values" "Unit")
    '("SIrange" [ "Options" ] "Value 1" "Value 2" "Unit")
    '("sisetup" "Options")
    '("tablenum" [ "Options" ] "Number")
    '("DeclareSIUnit" [ "Options" ] "Unit" "Symbol")
    '("DeclareSIPrefix" "Prefix" "Symbol" "Powers of 10")
    '("DeclareBinaryPrefix" "Prefix" "Symbol" "Powers of 2")
    '("DeclareSIPostPower" "Name" "Power")
    '("DeclareSIPrePower" "Name" "Power")
    '("DeclareSIQualifier" "Qualifier" "Symbol"))
   (TeX-run-style-hooks "l3keys2e"
			"array"
			"amstext"
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
   				("DeclareSIUnit" "[{{")
   				("DeclareSIPrefix" "{{{")
   				("DeclareBinaryPrefix" "{{{")
   				("DeclareSIPostPower" "{{")
   				("DeclareSIPrePower" "{{")
   				("DeclareSIQualifier" "{{"))
   			      'function))))

;; siunitx.el ends here
