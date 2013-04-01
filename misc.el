(defun TeX-add-to-alist (alist-var key value)
  "Add VALUE to the element with KEY of the ALIST-VAR."
  (let ((old-element (assoc key (symbol-value alist-var))))
    (if old-element
	(progn
	  (set alist-var (delete old-element (symbol-value alist-var)))
	  (set alist-var (add-to-list alist-var
				      (add-to-list 'old-element value t))))
      (set alist-var (add-to-list alist-var (list key value))))))
