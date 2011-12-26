;;; attempt three of SICP.

(defun hello-world1 (str)
  (format t "Hello ~A" str))

;; Square roots by newton's method
; to get the square root of a number, take a guess.
; then improve guess by averaging guess with x/guess till the
; guess is close to x/guess
(defparameter eps 0.000244)

;; This is original implementation of square root 
(defun square-root (x)
  (labels (
           (try (guess)
             (if (good-enough? guess)
                 guess
                 (try (improve guess))))
           (good-enough? (guess)
             (if (< (abs (- guess (/ x guess))) eps) guess nil ))
           (improve (guess)
             (average guess (/ x guess))))
    (try 1.0)))


(defun average (a b)
             (/ (+ a b) 2))

;; Factorial
(defun fact(n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))

(defun fact-iter (n)
  (iter 1 1 n))

(defun iter (product count n)
  (if (> count n)
      product
      (iter (* product count) (1+ count) n)))

(defun ackermann (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (t (ackermann (- x 1) (ackermann x (- y 1))))))

(defun na (m n)
  (cond ((= m 0) (+ n 1))
        ((and (> m 0) (= n 0)) (na (1- m) n))
        (t (na (- m 1) (na m (- n 1))))))

(defun fib (n)
  (cond ((= n 0)  0)
        ((= n 1) 1)
        (t (+ (fib (- n 2))
              (fib (- n 1))))))

(defun fib-iter (n)
  (labels (
           (fiter (a b count)
             (if (= count 0)
                 b
                 (fiter (+ a b) a (1- count)))))
    (fiter 1 0 n)))
(defun fib-loop (num)
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= num n) cur)))
;;; Counting change.
;   Algorithm: Number of ways to change amount A with coins N, if one of the coins is n,
;              then, NOW = NOW to change amount a with N-1 coins +
;              NOW to change amount (A-Dn) using N coins.

(defparameter coins '(50 25 10 5 1))
(defun count-change (amount)
  (labels (
           (count-change-rec (amt coins)
             (cond ((= amt 0) 1)
                   ((single? coins) 1)
                   ((< amt 0) 0)
                   (t (+ (count-change-rec amt (rest coins))
                         (count-change-rec (- amt (first coins)) coins))))))
    (count-change-rec amount coins)))
(defun single? (lst)
  (and (consp lst)
       (not (cdr lst))))

(defun count-change-rec (amt coins)
  (cond ((= amt 0) 1)
        ((single? coins) 1)
        ((< amt 0) 0)
        (t (+ (count-change-rec amt (rest coins))
              (count-change-rec (- amt (first coins)) coins)))))