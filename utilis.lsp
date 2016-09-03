(defun interactive-interpreter (prompt transfomer)
 "read a expression,tranform it,ans print the result"
 (loop
  (handler-case
   (progn
	(if (stringp prompt)
	 (print prompt)
	 (funcall prompt)
	)
	(print (funcall transfomer (read)))
	;;in case of error,do this:
	;(error (condition)
	; (format t "~&;; Error ~a ignored, back to top levele." condition)
	;)
   )
  )
 )
 )
(defvar *promptCount* 0)
(defun lisp ()
 (interactive-interpreter #'(lambda ()
							  (format t "~&[~a]>" *promptCount*) 
							  (incf *promptCount*)
							  ) 
#'eval)
 )


;for debug
(defvar *dbg-ids* nil "Identifiers used by dbg")
(defun dbg (id format-str &rest args)
  "Print debugging info if (debug id) has been specified."
  (when (member id *dbg-ids*)
	(fresh-line *debug-io*)
	(apply #'format *debug-io* format-str args)
	)
  )
(defun dbg-indent (id indent format-str &rest args)
 "print indented debugging info if (debug id) has specified."
 (when (member id *dbg-ids*)
	(fresh-line *debug-io*)
	(dotimes (i indent) (princ " " *debug-io*))
	(apply #'format *debug-io* format-str args)
	)
 )

(defun debug (&rest ids)
 "start dbg output on the given ids."
 (setf *dbg-ids* (union *dbg-ids* ids))
 )
(defun undebug (&rest ids)
 "stop dbg on the ids"
 (setf *dbg-ids* (if (null ids) nil 
				  (set-difference *dbg-ids* ids)
				 )
 )
 )


(defun random-elt (l) 
  "random select one of l"
  (elt l (random (length l)))
  )
(defun mappend (f h)
  "map f on h and append them together"
  (apply #'append (mapcar f h))
  )

(defun find-all (item seq &rest keys &key (test #'eql) test-not &allow-other-keys)
  "find all "
  (if test-not 
	(apply #'remove item seq :test-not (complement test-not ) keys)
	(apply #'remove item seq :test (complement test) keys)
	)
  )

(defun nsetf (x y) 
  "change x to y" 
  (setf (first x) (first y)) 
  (setf (rest x) (rest y)) 
  x
  )
