;; ASSIGNMENT 2 COMP 348
;; Matthew Salaciak 29644490
;; lisp

;;Question 1
;; (defun triangle (x)
;; (if (integerp x) 
;;         (cond ((> x 0)
;;         (print (append(triangle (- x 1))(list x)))
;;              )         
;;         ) 


;;         (print   "decimal numbers are not valid input, please enter an integer")  

;; ))

(defun triangle (n)
(if (integerp n) 
        (loop for x from 1 to n
        do
        (setf number 1)
        (TERPRI)
           (loop  for y from 1 to x
        do
        (format t "~A "  number)
        (incf number)
        )
        )
        (print "decimal numbers are not valid input, please enter an integer") 
))
      
(triangle 5.5)     
(triangle 5)



;;Question 2
(setf P1 '(x1 y1))
(setf P2 '(x2 y2))

;; regular function
(defun distance (P1 P2)
   (print (sqrt (+ (expt (- (car P1) (car P2)) 2) (expt (- (car (cdr P1)) (car (cdr P2))) 2)))))

(distance (list 6 4)(list 8 2))

;;lambda function
((lambda (P11 P22)
(print (sqrt (+ (expt (- (car P11) (car P22)) 2) (expt (- (car (cdr P11)) (car (cdr P22))) 2))))) (list 6 4)(list 8 2))

;; the lambda implementation of the function is more efficient in terms of memory allocation because it is an anonymous function.
;; anonymous function are not stored in memory like defun is.



;; question 3


(defun mixList (lst)
    (cond 
    ((null lst) lst)
    ((listp (car lst)) (mixList (append (car lst) (cdr lst))))
    ((numberp (car lst)) (mixList (cdr lst)))
    (t (cons (car lst) (mixList (cdr lst))))
    )      
)

(defun mixListNoDups (lst)
(let ((L (mixList(list lst)))) 

  (cond ((null L) L)
        ((member (car L) (cdr L))
         (mixListNoDups (cdr L)))
        (t (cons (car L) (mixListNoDups (cdr L))))))
)

(print (mixListNoDups (list '((n) 2 (6 h 7.8) (w f) (n) (c) n))))
(print (mixListNoDups (list '((z f) (b a 5 3.5) 6 (7) (a) c))))

;; question 4

(defun f-l-swap (lst)


(cond ((null lst) null)
((null (cdr lst)) (cons (car lst) nil ))
(t (f-l-swap (cdr lst))      )
)

)

;; (print (f-l-swap '(a b c d e)))



;; question 5




;; (defun treeCompress (lst)
;;     (cond 
;;     ((null lst) lst)
;;     ((listp (car lst) ) (treeCompress (append (car lst) (cdr lst))))

;;     (t (cons (car lst) (treeCompress (cdr lst))))
;;     )      
;; )

;; (defun is-sortedp (lst)
;; (let ((L (treeCompress(list lst))))

;; (cond ((numberp (car L))
;; (cond ((or (null L) (null (cdr L))) t)
;; ((< (car L) (car (cdr L))) (is-sortedp (cdr L)))
;; ((= (car L) (car (cdr L))) (is-sortedp (cdr L)))
;; (t nil))
;; (is-sortedp (cdr lst)))
;; )
;; )



;; (print (is-sortedp '(1 '(1) 4)))

;; (print (treeCompress '(1 '(1) 4)))




;;question 7

;; recursive approach


(defun pellnumbers (n)
(setf first 0)
(setf second 1)
  (cond
    ((zerop n) first ) 
    ((= n 1) second)
    (T
     (let ((a (* 2 (pellnumbers (- n 1) )))
           (b (pellnumbers (- n 2) )))
           (+ a b)) 
     )))
  


(defun pell-recursive (n)
    (setf pellnumberlist '())
    (loop for x from 0 to n
        do (
            setf pellnumberlist (append pellnumberlist (list (pellnumbers x)))
        )
    )
   (print pellnumberlist) 
)
      

(defun pell-iterative (n)
(let* (
(firstnum 0)
(secondnum 1)
(pellnumberslist  (list firstnum secondnum))
(thirdnum secondnum))

(loop for x from 2 to n
do 
(setf thirdnum secondnum)
(setf secondnum (+ (* 2 secondnum) firstnum))
(setf pellnumberslist (append pellnumberslist (list secondnum)))
(setf firstnum thirdnum)
)
(print pellnumberslist)
)

)





(pell-recursive 6)
(pell-recursive 8)
(pell-iterative 10)
(pell-iterative 12)







