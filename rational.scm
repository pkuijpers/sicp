(define (println line) (display line) (newline))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (make-rat n d)
	(if (< d 0)
		(make-rat (* -1 n) (* -1 d))
		(let ((g (abs (gcd n d))))
	 		(cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))

(define (add-rat x y)
	(make-rat (+ (* (numer x) (denom y))
		         (* (numer y) (denom x)))
	          (* (denom x) (denom y))))

(define (sub-rat x y)
	(make-rat (- (* (numer x) (denom y))
		         (* (numer y) (denom x)))
	          (* (denom x) (denom y))))

(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
		      (* (denom x) (denom y))))

(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
		      (* (denom x) (numer y))))

(define (equal-rat? x y)
	(= (* (numer x) (denom y))
	   (* (numer y) (denom x))))

(define one-third (make-rat 1 3))
(define one-half (make-rat 1 2))
(define one (make-rat 1 1))
(define minus-one (make-rat -1 1))

(print-rat (add-rat one-third one-third))

; Test 2.1
(print-rat (make-rat 1 2))	; 1/2
(print-rat (make-rat -1 2))	; -1/2
(print-rat (make-rat 1 -2))	; -1/2
(print-rat (make-rat -1 -2))	; 1/2
(print-rat (mul-rat one-third minus-one))	; -1/3
(print-rat (mul-rat minus-one minus-one))	; 1/1

(newline)