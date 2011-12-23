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