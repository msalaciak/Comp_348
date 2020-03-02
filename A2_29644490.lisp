;; ASSIGNMENT 2 COMP 348
;; Matthew Salaciak 29644490
;; lisp

;;Question 1
(defun triangle (x)
(if (integerp x) 
        (cond ((> x 0)
        (print (append(triangle (- x 1))(list x)))
             )         
        ) 
        (print (list x "decimal numbers are not valid input, please enter an integer"))  

))
      
         
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

;; question 3












     
