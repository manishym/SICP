;;;; Chapter 2 of SICP
;      data 
(defun make-point (xcor ycor)
  (cons xcor ycor))
(defun xcor (point)
  (car point))
(defun ycor (point)
  (cdr point))
(defun make-seg (start-point end-point)
  (cons start-point end-point))
(defun start-point (segment)
  (car segment))
(defun end-point (segment)
  (cdr segment))
(defun mid-point (segment)
  (let ((start (start-point segment))
	(end (end-point segment)))
    (labels ((mid (start end)
	       (make-point (average (xcor start) (xcor end))
			   (average (ycor start) (ycor end)))))
      (mid start end))))
(defun print-point (point)
  (format t "(~A, ~A)" (xcor point) (ycor point)))
(defun distance-between-points (x y)
  (sqrt (+ (square (- (xcor x) (xcor y)))
	   (square (- (ycor x) (ycor y))))))

(defun length-seg (segment)
  (distance-between-points (start-point segment) (end-point segment)))
(defun make-rectange (length-seg breadth-seg)
  (cons length-seg breadth-seg))
(defun area-of-rectangle (rectangle)
  (* (length-seg  (car rectangle))
     (length-seg (cdr rectangle))))


;;;; Well well well.....
;     Till now I have been thinking that cons should be implemented like malloc.
;     This shows real power of lisp. Since we can generate functions in lisp at
;     runtime, we can write cons using lambda. Here is implementation in CL
(defun new-cons (x y)
  (lambda (num)				; simply create a function, which called
					; with arg 1, returns first mem, with
					; arg 2 returns second number. Now define
					; car and cdr :-)
    (cond ((= num 1) x)
	  ((= num 2) y)
	  (t nil))))

(defun new-car (lst)
  (funcall lst 1))

(defun new-cdr (lst)
  (funcall lst 2))


;;; Extended exercise: Interval arithmetic
;    Interval arithmetic is way of calculating in intervals. For example a resistance
;    It is the upper bound and lower bound we get when we calculate the resistance.
;
;
(defun make-interval (lower upper)
  (cons lower upper))

(defun upper-bound (interval)
  (cdr interval))
(defun lower-bound (interval)
  (car interval))


(defun add-interval (a b)
  (make-interval (+ (lower-bound a) (lower-bound b))
		 (+ (upper-bound a) (upper-bound b))))
(defun mul-interval (a b)
  (let ((p1 (* (lower-bound a) (lower-bound b)))
	(p2 (* (lower-bound a) (upper-bound b)))
	(p3 (* (upper-bound a) (lower-bound b)))
	(p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(defun div-interval (a b)
  (mul-interval a
		(reciprocate-interval b)))
(defun sub-interval (a b)
  (make-interval (- (lower-bound a) (upper-bound b))
		 (- (upper-bound a) (lower-bound b))))
(defun reciprocate-interval (int)
  (if (or (zerop (upper-bound int))
	  (zerop (lower-bound int)))
      (error "reciprocate-interval: division by zero")
      (make-interval (/ 1.0 (upper-bound int))
		     (/ 1.0 (lower-bound int)))))
(defun make-center-width (center width)
  (make-interval (- center width)
		 (+ center width)))
(defun width-of-interval (int)
  (/ (- (upper-bound int) (lower-bound int)) 2))
(defun make-center-percent (center percent)
  (make-interval (- center (* center (/ percent 100)))
		 (+ center (* center (/ percent 100)))))
(defun percent-of-interval (int)
  (* (/ (width-of-interval int) (center-of-interval int)) 100.0))


;;; Lists
;    ex 2.17 define last-pair, that returns last element as a list
(defun last-pair (lst)
  (cond ((null lst) nil)
	((null (cdr lst)) lst)
	(t(last-pair (cdr lst)))))
;     ex 2.18, reverse a list
(defun reverse-list (lst)
  (if (null lst)
      nil
      (append (reverse-list (cdr lst)) (list (car lst)))))
;; ex 2.19, already done in chapter 1
;; ex 2.20 is scheme specific

(defun map-list (lst fun)
  (if (null lst)
      nil
      (cons (funcall fun (car lst)) (map-list (cdr lst) fun))))
;; ex 2.21 define square list using map-list and with out using
(defun square-list-wo (lst)
  (if (null lst)
      nil
      (cons (square (car lst)) (square-list-wo (cdr lst)))))
(defun square-list (lst)
  (map-list lst #'square))
;; 2.22, since that bugger is consing answer, rather than what is returned, we get a reverse list. Second option will give a dotted list. He should use append instead.
;; 2.23, for list
(defun for-list (func list)
  (if (null list)
      nil
      (progn
	(funcall func (car list))
	(for-list func (cdr list)))))