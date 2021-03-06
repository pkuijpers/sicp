# SICP Solutions - Chapter 2 #

## Exercise 2.1 ##

	(define (make-rat n d)
		(if (< d 0)
			(make-rat (* -1 n) (* -1 d))
			(let ((g (abs (gcd n d))))
		 		(cons (/ n g) (/ d g)))))

## Exercise 2.2 ##
	(define (midpoint-segment s)
		(make-point (/ (- (x-point (end-segment s)) (x-point (start-segment s))) 2)
			(/ (- (y-point (end-segment s)) (y-point (start-segment s))) 2)))

## Exercise 2.3 ##
Perimeter and area are calculated in terms of width and height of the rectangle:

	(define (perimeter rect)
		(+ (* 2 (height rect)) (* 2 (width rect))))
	(define (area rect)
		(* (height rect) (width rect)))

One representation, based on an origin point and scalars for width and height:

	(define (make-rect origin height width)
		(cons origin (cons height width)))
	(define (origin rect) (car rect))
	(define (height rect) (car (cdr rect)))
	(define (width rect) (cdr (cdr rect)))

Another, where the rectangle is defined by its top-left and bottom-right point:

	(define (make-rect topleft bottomright)
		(cons topleft bottomright))
	(define (origin rect) (car rect))
	(define (height rect) (abs (- (y-point (cdr rect)) (y-point (car rect)))))
	(define (width rect) (abs (- (x-point (cdr rect)) (x-point (car rect)))))

## Exercise 2.4 ##
`cons` returns a procedure that takes another procedure as arguments and applies `x` and `y` to that
procedure. `cdr` and `cons` execute that procedure with a procedure as argument that returns the first 
(for car) or second (for cdr) argument.

	(define (cdr z)
		(z (lambda (p q) q)))

## Exercise 2.5 ##
I spent some time trying to find a simple method to determine a and b from 2^a*2^b to finally find that
there is none. The solution is to iteratively find the number of times n is divisable by 2 or 3. Thanks to [Bill the Lizard](http://www.billthelizard.com/2010/10/sicp-25-representing-pairs-as-product.html).

	(define (cons x y)
		(* (expt 2 x) (expt 3 y)))

	; Determine the number of times n is dividable by f
	(define (num-divided n f)
		(if (= (remainder n f) 0)
			(+ 1 (num-divided (/ n f) f))
			0))

	(define (car z)
		(num-divided (/ z (expt 3 (num-divided z 3))) 2))
	(define (cdr z)
		(num-divided (/ z (expt 2 (num-divided z 2))) 3))

## Exercise 2.6 ##
Mind-boggling. Got to:

	; (define one (add-1 zero))
	(define one (lambda (f) (lambda (x) (f (lambda (x) x) x))))

then gave up and looked up the [answer from Bill the Lizard](http://www.billthelizard.com/2010/10/sicp-26-church-numerals.html). It didn't occur to me that the previous method could be simplified to 

	(define one (lambda (f) (lambda (x) (f x))))

## Exercise 2.7 ##
Easy.

	(define (upper-bound x) (cdr x))
	(define (lower-bound x) (car x))

## Exercise 2.8 ##
	(define (subtract-interval x y)
  	(make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

## Exercise 2.9 ##

Interval 1: l1 to u1
Width interval 1 = (u1-l1)/2 = w1
Interval 2: l2 to u2 
Width interval 2: (u2-l2)/2 = w2
Sum: (l1 + l2) to (u1 + u2)
Width of sum is ((u1+u2)-(l1+l2))/2
<=> (u1+u2-l1-l2)/2
<=> (u1-l1+u2-l2)/2
<=> (u1-l1)/2 + (u2-l2)/2
<=> w1 + w2

Same goes for difference: (l1-u2) to (u1-l2)
Width of difference is ((u1-l2)-(l1-u2))/2
<=> (u1-l2-l1+u2)/2 <=> (u1-l1+u2-l2)/2 <=> w1 + w2

Width of multiplication is not a function of the widths of the arguments:
(1 to 2) * (2 to 4) = (2 to 8), width = 3
(2 to 3) * (3 to 5) = (6 to 15), width =  4.5 while the widths of the arguments are the same as previous.

## Exercise 2.10 ##
When we divide by an interval that spans zero it could be a divide by zero. Therefore we can't divide by
an interval that includes zero.

	(define (div-interval x y)
	  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
	    (error "Divide by interval spanning zero" y))
	  (mul-interval x
	    (make-interval (/ 1.0 (upper-bound y))
	                   (/ 1.0 (lower-bound y)))))
## Exercise 2.11 ##
    ; Checks if the interval is all negative (-1), all positive (1) or spanning 0 (0)
    (define (sign-interval x)
     (cond ((<= (upper-bound x) 0) -1)
      ((>= (lower-bound x) 0) 1)
      (else 0)))

    (define (neg-interval? x)
      (= -1 (sign-interval x)))

    (define (pos-interval? x)
      (= 1 (sign-interval x)))

    (define (zero-interval? x)
      (= 0 (sign-interval x)))

    (define (mul-interval x y)
     (cond 
          ((and (pos-interval? x) (pos-interval? y)) ; 1
            (make-interval (* (lower-bound x) (lower-bound y))
                           (* (upper-bound x) (upper-bound y))))
          ((and (zero-interval? x) (pos-interval? y)) ; 2
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (upper-bound x) (upper-bound y))))
          ((and (neg-interval? x) (pos-interval? y)) ; 3
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (upper-bound x) (lower-bound y))))
          ((and (pos-interval? x) (zero-interval? y)) ; 4
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (upper-bound x) (upper-bound y))))
          ((and (zero-interval? x) (zero-interval? y)) ; 5
           (let ((p1 (* (lower-bound x) (lower-bound y)))
                 (p2 (* (upper-bound x) (upper-bound y)))
                 (p3 (* (lower-bound x) (upper-bound y)))
                 (p4 (* (upper-bound x) (lower-bound y))))
            (make-interval (min p3 p4)
                           (max p1 p2))))
          ((and (neg-interval? x) (zero-interval? y)) ; 6
           (make-interval (* (lower-bound x) (upper-bound y))
                          (* (lower-bound x) (lower-bound y))))
          ((and (pos-interval? x) (neg-interval? y)) ; 7
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (lower-bound x) (upper-bound y))))
          ((and (zero-interval? x) (neg-interval? y)) ; 8
           (make-interval (* (upper-bound x) (lower-bound y))
                          (* (lower-bound x) (lower-bound y))))
          ((and (neg-interval? x) (neg-interval? y)) ; 9
           (make-interval (* (upper-bound x) (upper-bound y))
                          (* (lower-bound x) (lower-bound y))))))

## Exercise 2.12 ##

    (define (make-center-percent c p)
      (make-center-width c (* (/ p 100) c)))

    (define (percent i)
      (* (/ (- (center i) (lower-bound i)) (center i)) 100))

## Exercise 2.13 ##

    (a ± i%) * (b ± j%)
    
    Lower bound = (a - i%) * (b - j%)
    <=> (a - ia/100) * (b - jb/100)
    <=> a * b - (iba/100) - (jba/100) + (ijab/10000)
    ≈ a * b - (iba/100) - (jba/100)
    <=> a * b - ((j+i)ba/100)
    <=> a * b - (j + i)%

Same way for lower bound:
    
    a * b + (j + i)%

So:
    
    (a ± i%) * (b ± j%) ≈ (a * b) ± (i + j)%
