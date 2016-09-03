;gps.lsp General Problem Solver
;author eric
;How to run:
;	(test 0)
;	(test 1)
;	(test 2)
;
;(defun complement(fn) #'(lambda (&rest args) (not (apply fn args))))
(defun one-myAnd (a &rest b) (if a (if b (apply #'one-myAnd b) a) nil))
(defun myAnd (&rest l) 
 "return t when l=nil"
 (if (null l) t (apply #'one-myAnd  l)))

(defun find-all (item seq &rest keys &key (test #'eql) test-not &allow-other-keys)
  (if test-not (apply #'remove item seq :test-not (complement test-not ) keys)
    (apply #'remove item seq :test (complement test) keys)))

(defun nsetf (x y) (setf (first x) (first y)) (setf (rest x) (rest y)) x)


(defvar *cs*)
(defun GPS (cs goals ops ) 
  (setf *ops* ops)
  (setf *cs* (copy-tree cs))
      (if (every #'(lambda (p) (achieve *cs* p)) goals) (format t "~&Solved") (format t "~&Can't accomplish!"))
  ;  (let ((succ t))
  ;    (dolist (goal goals) (when (not (achieve cs goal)) (setf succ nil)))
  ;    (if succ (format t "~&Solved") (format t "~&Can't accomplish!"))
  ;    )
  )

(defun achieve (cs goal) 
  "find a appropriate op in ops to achieve goal,and return op,t represent do nothing"
  (if (member goal cs) t
  (let ((appro-ops (find-all goal *ops* :test #'appropriate-p )))
    (if appro-ops (find cs appro-ops :test #'apply-op) nil)
    )
  )
  )

(defun apply-op (cs op)
  (if  (apply #'myAnd (mapcar #'(lambda (p) (achieve cs p)) (op-preconds op)))
    (progn (format t "~&executing ~a" (op-action op))
           (nsetf cs (union (set-difference cs (op-del-list op)) (op-add-list op))) 
           )

    nil)
  )

(defstruct op action
  (preconds nil)
  (add-list nil)
  (del-list nil)
  )

(defun op-p (op)
  (and (vectorp op)  (eq (elt op 0) 'op))
  )
(defun appropriate-p (goal op)
  (member goal (op-add-list op))
  )
(defparameter *school-ops*
  (list 
    (make-op :action 'drive-son-to-school
             :preconds '(son-at-home car-works)
             :add-list '(son-at-school)
             :del-list '(son-at-home)
             )
    (make-op :action 'shop-installs-battery
             :preconds '(car-needs-battery shop-knows-problem shop-has-money)
             :add-list '(car-works)
             )
    (make-op :action 'tell-shop-problem
             :preconds '(in-comunication-with-shop)
             :add-list '(shop-knows-problem)
             )
    (make-op :action 'telephone-shop
             :preconds '(know-phone-number)
             :add-list '(in-comunication-with-shop)
             )
    (make-op :action 'look-up-phone-number
             :preconds '(have-phone-book)
             :add-list '(know-phone-number)
             )
    (make-op :action 'give-shop-money
             :preconds '(have-money)
             :add-list '(shop-has-money)
             :del-list '(have-money)
             )
    )
  )
(defvar *ops*)

;for test
(defstruct gps-test (cs nil) goal ops)
(defun run-test ( test)
(format t "~&runing (gps '~a '~a '~a)" (gps-test-cs test) (gps-test-goal test ) (gps-test-ops test ))
  (gps (gps-test-cs test) (gps-test-goal test ) (gps-test-ops test )
       ))
(setf test-cases 
      (list 
        (make-gps-test :cs '(son-at-home car-works) 
                       :goal '(son-at-school)
                       :ops *school-ops*
					   )
        (make-gps-test :cs '(son-at-home car-needs-battery have-money have-phone-book) 
                       :goal '(son-at-school)
                       :ops *school-ops*
					   )
        (make-gps-test :cs '(son-at-home car-needs-battery have-money ) 
                       :goal '(son-at-school)
                       :ops *school-ops*
					   )

      )
	  )
(defun test (n)
   (if (< n (length test-cases))
             (run-test (elt test-cases n))
              (format t "~&Over range of test-cases.")))


