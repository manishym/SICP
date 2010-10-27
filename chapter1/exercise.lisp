;;;;    The exercises in SICP book.
;;;;
;; 
;; ex 1.3. Return sum of squares of big two numbers of three.
;; 
(defparameter DELTA 0.0001)
(defvar dx 0.00001)
(defun sum-squares (a b)
(+ (square a)
   (square b)))
(defun square (x) (* x x))
(defun sum-squares-big-two-old (a b c)
  (cond ((>= a b)
	 (if ( >= c b)
	     (sum-squares a c)
	     (sum-squares a b)))
	((>= b a) 
	 (if ( >= c a)
	     (sum-squares b c)
	     (sum-squares b a)))
	((>= c a)
	 (if (>= a b)
	     (sum-squares a c)
	     (sum-squares b c)))  
	(t 'ERROR)))
(defun big-two (a b c)
  (cond ((>= a b)
	 (if ( >= b c)
	     (values a b)
	     (values a c)))
	(t (big-two b c a))))
(defun sum-squares-big-two (a b c)
  (let ((lst (multiple-value-list (big-two a b c))))
    (sum-squares (car lst) (cadr lst))))

;; ex 1.4: the operation (if (> b 0) + -) will itself reduce to + or - and then it
;;         will be applied
;;
;; ex 1.5: Since I do not have scheme, I cannot verify the answer, let me check
;;         funcall p will never be evaluated because x is 0. If it were normal
;;         order evaluation, the program would go to infinite recursion, since it
;;         would try to evaluate the args. I cannot verify the answer, since in CL,
;;         functions are in different namespace compared to SCHEME.
;; ex 1.6: This if will not work because, since it is function, its args are evaluated
;;         first and then function is called, so every time it tries to eval (improve)
;;         it keeps going into another improve.
;;;; square root by newton's method
;;
;;
;;
(defun square-root (x)
  (labels (
	   (try (num)
	     (if (good-enough? num)
		 num
		 (try (improve num))))
	   (good-enough? (root)
	     (if (<= (abs (- (square root) x)) DELTA)
		 root))
	   (improve (root)
	     (average root (/ x root)))
	   (average (a b) (/ (+ a b) 2)))
    (try 1.0)))
;; ex 1.8 Cube root by newton's method.
(defun cube-root (x)
		 (labels (
			  (try (num)
			    (if (good-enough? num)
				num
				(try (improve num))))
			  (good-enough? (root)
			    (if (<= (abs (- (cube root) x)) (delta))
				root))
			  (delta ()
			    ( * x DELTA))
			  (cube (x)
			    (* x x x))
			  (improve (root)
			    (/ (+ (/ x (square root)) (* 2 root) ) 3))
			  (average (a b) (/ (+ a b) 2)))
		   (try 1.0)))





;;;;;;;;;;;; Section 1.2
;;;;   Exercise 1.9. Each of the following two procedures defines a method for adding
;;     two positive integers in terms of the procedures inc, which increments
;;     its argument by 1, and dec, which decrements its argument by 1.
;;     ANS: First one is recursive and second iterative.


;; Ex 1.10
;; (defun A (x y)
;;   (cond ((= y 0) 0)
;;         ((= x 0) ( * 2 y))
;;         ((= y 1) 2)
;;         (t (A (- x 1) (A x (- y 1))))))
;;  (A 1 10)
;;  (A 0 (A 1 9)
;;        (A 0 (A 1 8)
;;             (A 0 (A 1 7)
;;                  (A 0 (A 1 6)                         
;;                       (A 0 (A 1 5)                    (A 0 32)
;;                            (A 0 (A 1 4)               (A 0 16)
;;                                 (A 0 (A 1 3)          (A 0 8)
;;                                      (A 0 (A 1 2)     (A 0 4)
;;                                           (A 0 2)


;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 0 (A 2 2)))
;; (A 1 (A 0 (A (defun A (x y)
(defun A (x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(t (A (- x 1) (A x (- y 1))))))


;;;; Defined fib iter, now the challenge is to write a iter function to
;;   calculate the number of ways one can change a certain amount
;;   using certain denominations
(defparameter denoms '(100000 50000 10000 1000 100 50 25 10 5))
(defun number-of-ways-to-change (amount denoms)
  (cond  ((= amount 0) 1)
	 ((< amount 0) 0)
	 ((null denoms) 0)
	 (t (+ (number-of-ways-to-change (- amount (first denoms)) denoms)
	       (number-of-ways-to-change amount (rest denoms))))))
;;;;; Ex 1.11
;;    f(n) = n for n < 3
;;    f(n) = f(n-1) + 2 f(n-2) + 3 f (n-3) for n >= 3
;;    write a recursive as well as iterative definition for f(n)
(defun iter (count fn1 fn2 fn3 n)
  (cond  ((< n 3) n)
	 ((>= count n) fn1)
	 (t (iter (+ 1 count) (+ fn1 (* 2 fn2) (* 3 fn3)) fn1 fn2 n))))


(defun pascal-row (previous-row current-row)
  (cond ((null previous-row) (append '(1) current-row))
	((null (second previous-row)) (append '(1) current-row))
	(t  (append (list (+ (first previous-row) (second previous-row)))
		    (pascal-row (rest previous-row) current-row)))))
(defun pascal-row-full (lst)
  (cons 1 (pascal-row lst nil)))
;;;;; Higher order procedures
(defun sumint (a b)
  (if (> a b)
      0
      (+ a (sumint (+ a 1) b))))
(defun cube (a) (* a a a))
(defun sumcubeint (a b)
  (if (> a b)
      0
      (+ (cube a) (sumcubeint (+ a 1) b))))
(defun generic-sum (a b func next)
  (if (> a b)
      0
      (+ (funcall func a) (generic-sum 
			   (funcall next a) b func next))))

(defun accumulator (a b func next operation )
  (if (> a b)
      (funcall operation)
      (funcall operation (funcall func a) (accumulator 
					   (funcall next a) b func next operation))))
(defun pi-sum (a b)
  (accumulator a b #'(lambda (x) (/ 1.0 (* x (+ x 2))))
	       #'(lambda (x) (+ x 4)) #'+))
(defun pi-prdt (a b)
  (ACCUMULATOR a b #'(lambda (x) (/ (* x  (+ x 2)) (square (+ x 1))))
	       #'(lambda (x) (+ x 2)) #'*))
(defun accumulator-iter (a b func next operation )
  (labels ((iter (result a)
	     (if (> a b)
		 result
		 (funcall operation
			  (iter (funcall 
				 operation 
				 result 
				 (funcall func a)) (funcall next a))))))
    (iter (funcall operation) a)))

;;; Since filtered accumulator is pretty useful and can be confusing to use
;     here are examples
;     For sum of integers from a to b, define like this
;     (defun sumint (a b)
;       (filtered-accumulator a b #'(lambda (x) (x)) #'1+ #'+ #'numberp)
;       this means sum of a to b, with not doing any operation on a before summing
;       and getting next operand by incrementing a, next + is for sum, put a * if
;       you want product, next is filter, simply if it is a number, its enough.
;       Another example, to find sum of squares of every prime fron a to be
;       (filtered-accumulator a b #'square  1+ #'+  #'primep)
;       product of sum of every odd number and the number next to it from a  to b
;       for example (1 + 2) * (3 + 4) * (5 + 6) * (7+8)
;       (filtered-accumulator a b #'(lambda (x) (+ x x 1)) #'1+ #'* #'oddp)
(defun filtered-accumulator (a b func next operation filter)
  (cond ((> a b) (funcall operation))
	((funcall filter a) (funcall operation
				     (funcall func a) 
				     (filtered-accumulator (funcall next a)
							   b func next
							   operation
							   filter)))
	(t (filtered-accumulator (funcall next a)
				 b func next operation filter))))



(defun epower (num pow)
  (cond ((< 0 pow) nil)
	((= 0 pow) 1)
	((= 1 pow) num)
	((evenp pow) (square (epower num (/ pow 2))))
	((oddp pow) (* num (epower num (- pow 1))))))

(defun fermat-test (num)
  (labels ((try (a)
	     (= (expmod a num num) a)))
    (try (+ 1 (random (- num 1))))))
(defun fast-prime? (num times)
  (cond ((= 0 times) num)
	((fermat-test num) (fast-prime? num (- times 1)))
	(t nil)))
(defun expmod (num pow mod)
  (cond ((= pow 0) 1)
	((evenp pow) (mod (square (expmod num (/ pow 2) mod)) mod))
	(t (mod (* num (expmod num (- pow 1) mod)) mod))))


(defun non-trival-sqrt? (num mod)
  (cond ((= num 1) num)
	((= num (- mod 1)) num)
	(t (let ((rt (mod (square num) mod)))
	     (if (or (= rt 1) (= rt (- mod 1)))
		 0
		 num)))))
(defun generic-fast-prime? (num times test)
  (cond ((= 0 times) num)
	((not (numberp num)) nil)
	((funcall test num) (generic-fast-prime? num (- times 1) test))
	(t nil)))




(defun mr-expmod (num pow mod)
  (cond ((= pow 0) 1)
	((evenp pow) (let ((mrroot (mod 
				    (square 
				     (mr-expmod num (/ pow 2) mod )) 
				    mod)))
		       (non-trival-sqrt? mrroot mod)))
	(t (mod (* num (mr-expmod num (- pow 1) mod)) mod))))
(defun mr-test (num)
  (generic-prime-test num #'mr-expmod))
(defun generic-prime-test (num expmod)
  (labels ((try (a)
	     (= (funcall expmod a num num) a)))
    (try (+ 1 (random (- num 1))))))
(defun primep (num)
  (mr-fast-prime num (floor (/ num 2))))
(defun mr-fast-prime (num times)
  (cond ((= num 2) num) 
	((evenp num) nil)
	(t (generic-fast-prime? num times #'mr-test))))
(defun nth-prime (num n)
  (if (= n 0) num
      (nth-prime (next-prime num) (- n 1))))
(defun next-prime (num)
  (cond 
    ((= num 2) 3)
    ((evenp num) nil)
    ((primep (+ num 2)) (+ num 2))
    (t (next-prime (+ num 2)))))

;;;; Netwon's Method
;        The square root by successive approximation is a special case of newton's method
;        What newton's method says is
;        "If g(x) is a differentiable function of x, then a solution to g(x) = 0 is
;        fixed point of x -> f(x), where f(X) is
;        f(x) = x - g(x) / Dg(x); where Dg(x) is differential of g(x) at point x."
;        
(defun deriv(g) 
  (lambda (x)
    (/ (- (funcall g ( + x dx)) (funcall g x)) dx)))
(defvar *delta* 0.00001)
(defun fixed-point (f guess)
  (labels ((close-enough (val1 val2)
	     (< (abs (- val1 val2)) *delta*))
	   (try (guess)
	     (let ((next (funcall f guess)))
	       (if (close-enough guess next)
		   next
		   (try next)))))
    (try guess)))
(defun average-damp (f)
  (lambda (x) (average x (funcall f x))))
(defun average (num1 num2)
  (/ (+ num1 num2) 2))

(defun newtons-transform (g)
  (lambda (x) (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newtons-transform g) guess))
(defun square-root-newton (num)
  (newtons-method #'(lambda (x) (- (square x) num)) 1.0))

;; ex 1.37 infinite continued fraction
(defun cont-frac (n d k)
  (cond ((= k 0) (/ (funcall n k) (funcall d k)))
	(t (/ (funcall n k) (+ (funcall d k) (cont-frac n d (1- k)))))))

;; ex 1.43
(defun repeated (f n)
  (lambda (x)
    (cond ((zerop n) nil)
	  ((= n 1) (funcall f x))
	  (t (funcall (repeated f (1- n)) (funcall f x))))))
(defun repeated-using-compose (f n)
  (lambda (x)
    (cond ((zerop n) nil)
	  ((= 1 n) (funcall f x))
	  (t (funcall (compose (repeated-using-compose f (1- n)) f) x)))))
;; ex 1.42 double function, which applies a function of one arg twice
(defun compose (f g)
  (lambda (x)
    (funcall f (funcall g x))))
;; ex 1.41
(defun doublef (f)
  (lambda (x) 
    (funcall f (funcall f x))))

;; ex 1.44 smooth is a DSP function to smooth out a signal. This is done by averaging
;   f(x-dx) f(x) f(x+dx). Define a function smooth, which given a procedure, returns
;   procedure to smooth the function
(defun smooth (f)
  (labels ((average (a b c) (/ (+ a b c) 3)))
    (lambda (x) 
      (average (funcall f x) (funcall f (+ x dx)) (funcall f (- x dx))))))
(defun n-fold-smooth (f n)
  (repeated (smooth f) n))
(defun 4th-root (x)
  (fixed-point (doublef (average-damp #'(lambda (y) (/ x (cube y))))) 1.0))
(defun pow (base exp)
  (cond ((< exp 0) nil)
	((zerop exp) 1)
	(t (* base (pow base (1- exp))))))

