;(defun complement(fn) #'(lambda (&rest args) (not (apply fn args))))
(defun one-myAnd (a &rest b) (if a (if b (apply #'one-myAnd b) a) nil))
(defun myAnd (&rest l) 
 "return t when l=nil"
 (if (null l) t (apply #'one-myAnd  l)))

(defun find-all (item seq &rest keys &key (test #'eql) test-not &allow-other-keys)
  (if test-not (apply #'remove item seq :test-not (complement test-not ) keys)
    (apply #'remove item seq :test (complement test) keys)))

(defun nsetf (x y) (setf (first x) (first y)) (setf (rest x) (rest y)) x)


(defun GPS (cs goals ops ) 
  (setf *ops* ops)
    (let ((succ t))
      (dolist (goal goals) (when (not (achieve cs goal)) (setf succ nil)))
      (if succ (format t "~&Solved") (format t "~&Can't accomplish!"))
      )
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
  (if (or (subsetp (op-preconds op) cs) (apply #'myAnd (mapcar #'(lambda (p) (achieve cs p)) (op-preconds op))))
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
  (gps (gps-test-cs test) (gps-test-goal test ) (gps-test-ops test )
       ))
(setf test-cases 
      (list 
        (make-gps-test :cs '(son-at-home car-works) 
                       :goal '(son-at-school)
                       :ops *school-ops*)
        )
      )
(defun test (n)
   (if (< n (length test-cases))
             (run-test (elt test-cases n))
              (format t "~&Over range of test-cases.")))






(defun printSen (l &rest rl)
  "Oupt a sentences,with capitalized the first word,and a period in the end"
  ; (princ "l=") (princ l) (princ "rl=") (princ rl)
  (typecase l 
    (null )
    (list (format t "~&~@(~{~a~^ ~}~)." l) (when rl (apply #'printSen rl)))
    (atom (format t "~&~@(~{~a~^ ~}~)." (cons l rl)))
    )
  (values ))
(defmacro printS (&rest l) (printSen l))

(defun question (x op y)
  (format t "~&How much is ~d ~a ~d ?" x op y)
  (if (equal (read) (funcall op x y)) (princ "Correct!") (princ "Sorry,that's not right.")))

(defun math-quiz (op range n)
  (dotimes (i n)
    (question (random range) op (random range))
    )
  )


(defun dprint(x)
  "Print x in dotted pair notation"
  (cond ((atom x) (princ x))
        (t (princ "(")
           (dprint(first x))
           (princ ".")
           (dprint(rest x))
           (princ ")")
           x 
           )))

(defun generate-all (phrase)
  "generate a list of all possible expansions of this phrase"
  'a
  )


(defun combine-all (xl yl)
  (mappend #'(lambda (adden) (mappend #'(lambda (added) (list (append added adden))) xl)) yl))

(defun generate-tree (phrase)
  (cond ((listp phrase) (mapcar #'generate-tree phrase))
        ((rewrites phrase) (cons phrase  (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase)))
  )

(defun random-elt (l) (elt l (random (length l))))
(defun mappend (f h) (apply #'append (mapcar f h)))
(defun rule-lhs (rule) (first rule))

(defun rule-rhs (rule) (rest (rest rule)))

(defun rewrites (category)
  "return the possible rewrites rule for category"
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "generate random phrase according to *grammar*"
  (cond ((listp phrase) (mappend #'generate phrase))
        ((rewrites phrase) (generate (random-elt (rewrites phrase))))
        (t (list phrase)))
  )


(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase)
              )(noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  )
(defvar *grammar* *simple-grammar*)


(defparameter *bigger-grammar*
  '(
    (sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on )
    (Adj -> big little blue green adiabatic)
    (Article -> the a )
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)
    )
  )


(defun count-anywhere (a b) 
  (if (null b) 0 
    (
     if (atom (first b)) 
     (
      if (eql a (first b))
      (+ (count-anywhere a (rest b)) 1)
      (count-anywhere a (rest b))
      )
     (+ (count-anywhere a (first b)) (count-anywhere a (rest b)))
     )
    )
  )


(defun count-anywhere (a b) 
  (if (null b) 0 
    (
     if (eql a (first b))
     (+ (counter-anywhere a (rest b)) 1)
     (counter-anywhere a (rest b))
     )
    )
  )

/*****************My Note*************************
1.nil=()='nil='() ,and they are atom and list,
which means (listp nil ) and (atom nil) equal to t.

*************************************************/


