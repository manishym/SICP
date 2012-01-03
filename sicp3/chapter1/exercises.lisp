;;; Exercises in SICP book

;; A simple hello world to test

(defun hello-world (str)
  (format t "Hello ~A" str))

;Ex 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;Ex 1.3
(defun sum-big-square (a b c)
  (sum-square (big a b) (med a b c)))

(defun sum-square (a b)
  (+ (square a)
     (square b)))

(defun square (x)
  (* x x))

(defun big (a b)
  (if (> a b)
      a
      b))

(defun med (a b c)
  (if (= (big a b) (big b c))
      (big a c)
      (big b c)))


;Ex 1.4
; This example shows why scheme is elegant compared to common lisp.
; scheme implementation would be simply
; (defun a-p-a-p (if (> b 0) + -) a b)
; no funcall no ugly sharp-quotes
; common lisp is supposed to have more power when writing macros
; because of this saperation of function and data name spaces.
(defun a-plus-abs-b (a b)
  (funcall
   (if (> b 0)
       #'+
       #'-) a b))


;Ex 1.5 Normal order vs applicative order

(defun p ()
  (p))
(defun test (x y)
  (if (= x 0)
      y
      (p)))

;exercise 1.7 newtons method for square root.

(defun my-sqrt (x)
  (labels (
           (try (guess)
             (let ((improved (improve guess)))
               (if (good-enough? guess improved)
                   improved
                   (try improved))))
           (improve (guess)
             (average guess (/ x guess)))
           (good-enough? ( guess improved)
             (if (< (abs (- 1 (/ guess improved))) eps )
                 improved
                 nil)))
    (try 1.0)))
;; Newtons method for cuberoot x/(square y) + 2 y / 3
;  all i need to change is improve
(defun my-cube-rt (x)
  (labels (
           (try (guess)
             (let ((improved (improve guess)))
               (if (good-enough? guess improved)
                   improved
                   (try improved))))
           (improve (guess)
             (/ (+ (/ x (square guess)) (* 2 guess)) 3))
           (good-enough? ( guess improved)
             (if (< (abs (- 1 (/ guess improved))) eps )
                 improved
                 nil)))
    (try 1.0)))

;; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative
;; process.

(defun crazy-func (n)
  (cond ((< n 3) n)
        (t (+ (crazy-func (- n 1))
            (* 2 (crazy-func (- n 2) ))
            (* 3 (crazy-func (- n 3)))))))
(defun crazy-func-iter (n)
  (crazy-iter 2 1 0 3 n))


;;; iter alogrithm:
;     let us store cf of n-1, n-2 and n-3 as state variables. At any point, we can
;     return the caly of crazy-func if we have these values.
;     let a=n-1, b=n-2 c=n-3, cf(n) = a+2b+3c
;     cf(n+1) = a+2b+3c + a + b

(defun crazy-iter (a b c count n)
  (if (> count n)
      a
      (crazy-iter (+ a (* 2 b) (* 3 c)) a b (1+ count) n )))
;;; Counting change iter algorithm
;   1. when coins are empty and amount is zero, NOW=0
;   2. next stage is differenct for amount and coins.
;      1. When you add coin and amount is zero, NOW=1
;      2. When you add amount and coins are zero, NOW=0.
;   3. Rest is combination of for the above two.



;;; algorithm, given a previous row of pascal triangle, can I create next row.
;   (1 (+ first second) (second rest) 1)
(defun pascal-row (prev)
  (append '(1) (recurse prev)))

(defun recurse (prev)
  (if (single? prev)
      '(1)
      (append (list (+ (first prev) (second prev))) (recurse (rest prev)))))


(defun pascal-triangle (this-row next-row count current)
  (cond  ((= count current) (list this-row next-row '(1)))
         ((= current 1) '(1))
         (t
          (pascal-triangle this-row (pascal-row this-row) count (1+ current)) )))