(defun TeX-add-to-alist (alist-var new-alist)
  "Add NEW-ALIST to the ALIST-VAR.
If an element with the same key as the key of an element of
NEW-ALIST is already present, add the new values to it; if a
matching element is not already present, add the new element to
the front of ALIST-VAR."
  ;; Loop over all elements of NEW-ALIST.
  (while new-alist
    ;; Get the element of ALIST-VAR with the same key of the current element of
    ;; NEW-ALIST, if any.
    (let ((old-element (assoc (car (car new-alist)) (symbol-value alist-var))))
      (if old-element
	  (progn
	    (set alist-var (delete old-element (symbol-value alist-var)))
	    ;; Append to `old-element' the values of the current element of
	    ;; NEW-ALIST.
	    (mapc (lambda (elt) (add-to-list 'old-element elt t))
		  (cdr (car new-alist)))
	    (set alist-var (add-to-list alist-var old-element)))
	(add-to-list alist-var (car new-alist))))
    ;; Next element of NEW-ALIST.
    (setq new-alist (cdr new-alist))))
