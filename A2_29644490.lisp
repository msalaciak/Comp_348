;; ASSIGNMENT 2 COMP 348
;; Matthew Salaciak 29644490
;; Inspired by COMP 348 lectures, tutorial slides and principle of programming langauges textbook
;; running this entire program will output every test case / answer within the terminal


;;Question 1

(defun triangle (n)
(cond
        ((integerp n) 
        (loop for x from 1 to n
        do
        (setf number 1)
        (TERPRI)
        (loop  for y from 1 to x
        do
        (format t "~A "  number)
        (incf number)
        )))
        ((not (numberp n)) (print "strings are not valid input, please enter an integer") )
        (t  (print "decimal numbers are not valid input, please enter an integer"))))


;; test cases for question 1
(triangle "5")     
(triangle 5.5)     
(triangle 5)




;;Question 2


;; regular function
;; (P1 is x1 y1 P2 is x2 y2)

(defun distance (P1 P2)
   (print (sqrt (+ (expt (- (car P1) (car P2)) 2) (expt (- (car (cdr P1)) (car (cdr P2))) 2)))))

;;test case for question 2 - regular function
(distance '(6 4) '(8 2))

;;lambda function / test case
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

;;test cases for question 3
(print (mixListNoDups (list '((n) 2 (6 h 7.8) (w f) (n) (c) n))))
(print (mixListNoDups (list '((z f) (b a 5 3.5) 6 (7) (a) c))))

;; question 4

(defun f-l-swap (lst)
(setf first (list (car lst)))
(setf body (cdr lst))
(setf swaplist (list ()))
(setf counter 1)
(setf counter2 1)

(loop 
for x in body
do
(setf counter (+ 1 counter)))
(setf counter (- counter 1))

(loop 
for x in body 
do
(if (< counter2 counter)
(setf swaplist (append swaplist (list x))) )
(setf counter2 (+ 1 counter2))
(setf last x)
)
(append (list last) (cdr swaplist) first   )
)

;;test cases for question 4
(print (f-l-swap '((a d) f 10 w h)))
(print (f-l-swap '(g 6 p 10 m)))


;; question 5


(defun isBinarySearchTree (lst)
(setf root (car lst))
(setf leftTreeRoot (car (cdr lst)))
(setf rightTreeRoot (car (cdr (cdr lst))))

(cond
        ((null (car lst)) (return-from isBinarySearchTree t ))
        ((and (numberp (car leftTreeRoot))(< root (car leftTreeRoot))) (return-from isBinarySearchTree NIL) )
        ((and (numberp (car rightTreeRoot))(> root (car rightTreeRoot))) (return-from isBinarySearchTree NIL ) )
        (t  (return-from isBinarySearchTree (and (isBinarySearchTree leftTreeRoot) (isBinarySearchTree rightTreeRoot)) )
        ))
        )


;;test cases for question 3, 1st one is a binary tree, 2nd and 3rd are not
(print (isBinarySearchTree '(8 (3 (1 () ()) (6 (4 () ())( 7 () ()))) (10 (()) (14 (13) ())))))
(print (isBinarySearchTree '(8 (10 (1 () ()) (6 (4 () ())( 7 () ()))) (10 (()) (14 (13) ())))))
(print (isBinarySearchTree '(8 (3 (9 () ()) (6 (4 () ())( 7 () ()))) (10 (()) (14 (13) ())))))



;;question 6

;; this factorial function comes from page 194 from principle of programming language textbook
(defun factorial (n) 
(if (= n 0)
1
(* n (factorial (- n 1)))))


(defun sin-cos-comp (x n)

(setf sinx x)
(setf cosx 1)
(cond 
        ((not (numberp x)) (return-from sin-cos-comp "strings are not valid input, please enter a number for x"))
        ((not (numberp n)) (return-from sin-cos-comp "strings are not valid input, please enter a number for n"))
        ((not (integerp n)) (return-from sin-cos-comp "decimal numbers are not valid input, please enter an integer for n"))
        ((evenp n) 
            (loop for exponet from 1 to n
        do 
            (if (oddp exponet)
                (setf cosx (- cosx (/ (expt x (* 2 exponet)) (factorial (* 2 exponet)))))  
                (setf cosx (+ cosx (/ (expt x (* 2 exponet)) (factorial (* 2 exponet)))))  
            )
        ) 
        (return-from sin-cos-comp cosx))
        ((and (oddp n) (and (< -10 x) (> 10 x))) 
         (loop for exponet from 1 to n
         do 
            (if (oddp exponet)
                (setf sinx (- sinx (/ (expt x (+ 1 (* 2 exponet))) (factorial (+ 1 (* 2 exponet))))))  
                (setf sinx (+ sinx (/ (expt x (+ 1 (* 2 exponet))) (factorial (+ 1 (* 2 exponet))))))  
            )
        ) 
        (return-from sin-cos-comp sinx)
        )
        )
    )


;;test cases for sin-cis-comp

;;sinx function n is odd, x is between -10 and 10
(print (sin-cos-comp 3.0 3))
;;cosx function n is even
(print (sin-cos-comp 3.0 2))
;;sinx function n is odd, but x is not between -10 and 10 FAILS
(print (sin-cos-comp 11.0 3))
;;function rejects decimal value for n FAILS
(print (sin-cos-comp 11.0 3.0))
;;function rejects string values FAILS
(print (sin-cos-comp "11.0" "3.0"))




;;question 7

;; recursive approach

;;calculate pell numbers recursively
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
  

;; recursive approach - call pellnumbers function.
(defun pell-recursive (n)
    (setf pellnumberlist '())
    (loop for x from 0 to n
        do (
            setf pellnumberlist (append pellnumberlist (list (pellnumbers x)))
        )
    )
   (print pellnumberlist) 
)
      
;; iterative approach
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




;;two cases for each pell number function - recursive and iterative
(pell-recursive 6)
(pell-recursive 8)
(pell-iterative 10)
(pell-iterative 12)







