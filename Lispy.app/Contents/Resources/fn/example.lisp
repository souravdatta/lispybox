;;; Assuming fn module is loaded, check lispy/using to load in SBCL

;;; permutations of unique elements
(defun perms (lst)
  (cond
    ((null lst) '(nil))
    (t (fn:for ((x lst)
		(ps (perms (remove x lst))))
	       (cons x ps)))))


;;; permutations of list with duplicates
(defun perms2 (lst)
  (fn:$ ((mapcar #'(lambda (pl) (mapcar #'(lambda (e) (elt lst e)) pl)))
	 (perms)
	 (fn:range 0 (- (length lst) 1)))))


;;; permutations of string w/wo duplicates
(defun perms3 (s)
  (fn:$ ((mapcar #'(lambda (pl) (coerce pl 'string)))
	 (perms2)
	 (coerce s 'list))))

