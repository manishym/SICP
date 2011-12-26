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
