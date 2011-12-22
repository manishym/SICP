;;; Begining attempt three of SICP.

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