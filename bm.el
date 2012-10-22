;;; bm.el --- AUCTeX style for `bm.sty'

;; Author: Mosè Giordano <giordano.mose@libero.it>
;; Created: 2012-10-22
;; Keywords: tex

;;; Commentary:

;; This file adds support for `bm.sty'.

;;; Code:

(TeX-add-style-hook "bm"
 (lambda ()
   (TeX-add-symbols
    '("bm" 1))
   ;; Fontification
   (when (and (featurep 'font-latex)
   	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("bm" "{")
   				("hm" "{"))
   			      'bold-command))))

(defvar LaTeX-bm-package-options nil
  "Package options for the bm package.")

;; bm.el ends here
