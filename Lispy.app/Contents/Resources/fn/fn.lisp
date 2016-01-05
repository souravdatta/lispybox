(defpackage :fn
  (:use :common-lisp)
  (:export :for :range :fold-right :fold-left :filter
	   :curry :$))

(in-package :fn)

;;; FOR
;;; for is a crude implementation of list/array
;;; comprehension a-la Haskell with CL macros.
;;; NOTE: An inclusion-condition should be a simple boolean expression
;;; which is used to filter the list. It is _NOT_ a lambda or function but
;;; a simple Lisp expression consisting of iteration variables used in the
;;; same loop or outer loops.
;;; usage - (fn:for ((<var1> <list1> [<inclusion-condition>])
;;;                  (<var2> <list2> [<inclusion-condition>)...)
;;;                        <expression containing <var1> <var2> ...>)
;;; example -  (fn:for ((x '(1 2 3 4))
;;;                            (y (cons x '(A B C D))))
;;;                           (cons x y)) ===  ((1 . 1) (1 . A) (1 . B) (1 . C) (1 . D) 
;;;                                             (2 . 2) (2 . A) (2 . B) (2 . C) (2 . D) 
;;;                                             (3 . 3) (3 . A) (3 . B) (3 . C) (3 . D) 
;;;                                             (4 . 4) (4 . A) (4 . B) (4 . C) (4 . D))
;;; example -  (fn:for ((x '(1 2 3 4 5))
;;;		        (y '(1 2 3 4 5) (< y x))
;;;		        (z '(1 2 3 4 5) (< z y)))
;;;		       (list x y z)) === ((3 2 1) (4 2 1) (4 3 1) (4 3 2) 
;;;                                       (5 2 1) (5 3 1) (5 3 2) (5 4 1) 
;;;                                       (5 4 2) (5 4 3))
(defun recursive-build-dolist (args body result)
  "Aux function to create a (dolist (dolist ...) ... body) recursively"
  (cond
    ((null args) `(setf ,result (append ,result (list (progn ,body)))))
    (t (let* ((term (first args))
	      (sym (first term))
	      (lst (second term))
	      (cnd (third term)))
	 (if (not (null cnd))
	     `(dolist (,sym (remove-if-not #'(lambda (,sym) ,cnd) ,lst))
		,(recursive-build-dolist (rest args) body result))
	     `(dolist (,sym ,lst)   ;;; else
		,(recursive-build-dolist (rest args) body result)))))))

(defmacro for (args body)
  (let ((result (gensym)))
    `(let ((,result nil))
       ,(recursive-build-dolist args body result)
       ,result)))



;;; RANGE
;;; range generates a list of numbers
;;; usage - (range <end>) | (range <start> <end>) | (range <start> <end> <step>)
;;; example - (range 5) === '(0 1 2 3 4 5)
;;; example - (range 1 5) === '(1 2 3 4 5)
;;; example - (range 1 6 2) === '(1 3 5)
;;; 
(defun range (&rest args)
  (labels ((make-result (start end step) 
	     (if (not (and (numberp start)
			   (numberp end)
			   (numberp step)))
		 (error "Not all arguments are numbers")
		 (do*
		  ((i start (+ i step))
		   (result (list i) (cons i result)))
		  ((> i (- end step)) (reverse result))))))
    (let ((arg1 (first args))
	  (arg2 (second args))
	  (arg3 (third args)))
      (cond
	((and arg1 arg2 arg3) 
	 (make-result arg1 arg2 arg3))
	((and arg1 arg2 (not arg3))
	 (make-result arg1 arg2 1))
	((and arg1 (not arg2) (not arg3))
	 (make-result 0 arg1 1))
	(t nil)))))


;;; FOLD-RIGHT & FOLD-LEFT
;;; Fold a list with an element from right/left given a binary operator
;;; NOTE: Uses recursion and may have (the right one) stack overflow for a large list 
(defun fold-right (op elem lst)
  (cond
    ((null lst) elem)
    (t (funcall op 
		(first lst)
		(fold-right op elem (rest lst))))))


(defun fold-left (op elem lst)
  (cond
    ((null lst) elem)
    (t (fold-left op
		  (funcall op elem (first lst))
		  (rest lst)))))


;;; FILTER
;;; filters a list for a given predicate
(defun filter (pred lst)
  (remove-if-not pred lst))


;;; CURRY
;;; curry takes a function and returns a curried function for it
(defun curry (fn &rest args)
  #'(lambda (&rest more-args)
      (apply fn (append args more-args))))



;;; $ <experimental>
;;; This macro makes it easy to compose several partial functions
;;; together with one final complete function call
;;; Usage: (fn:compose (<partial-function-application-1>
;;;                     <partial-function-application-2>
;;;                     ...
;;;                     <complete-function-application>))
;;; Note: The arguments are passed inside a list much like LET
;;; Note: The last argument need to be a complete function application
;;; Note: Not much error handling as of now
;;; Example:
;;; (fn:$ ((append '(hello))
;;;        (mapcar #'first)
;;;        (list '(1 one) '(2 two)))) === (HELLO 1 2)
;;; Example:
;;; (fn:$ ((list 1 2 3))) === (1 2 3)
(defun gen-funcalls (exprs)
  (if (not (listp exprs))
      (error "First argument needs to be a list of composable functions"))
  (if (= (length exprs) 1)
      (first exprs)
      (let ((expr (first exprs)))
	`(funcall (curry #',(first expr)
			 ,@(rest expr))
		  ,(gen-funcalls (rest exprs))))))

(defmacro $ (exprs)
  (if (not (listp exprs))
      (error "First argument needs to be a list of composable functions"))
  (if (= (length exprs) 0)
      `(,@exprs)
      (gen-funcalls exprs)))
