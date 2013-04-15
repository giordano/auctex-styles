(let* ((extensions '("otf" "ttf" "OTF" "TTF" "ttc" "TTC" "dfont"))
       (TeX-search-files-type-alist '((fonts "${TTFONTS}" ("tex/") extensions))))
  (setq TeX-global-fonts
	(mapcar 'identity (TeX-search-files-by-type 'fonts 'global t nil))))
