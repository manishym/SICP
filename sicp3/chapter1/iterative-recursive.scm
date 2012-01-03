(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))
(define (+ a b)
  (if (= 0 a)
      b
      (+ (dec a) (inc b))))
(define (A x y) 
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y)) ((= y 1) 2) 
        (else (A (- x 1)
                 (A x (- y 1))))))
;; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure that computes f by means of a recursive process. Write a procedure that computes f by means of an iterative process.
