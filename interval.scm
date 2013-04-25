(require-extension srfi-78)

; SICP 2.1.4: Interval arithmetic
(define (println line) (display line) (newline))

(define (make-interval a b) (cons a b))

(define (equal-interval? a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (print-interval x)
  (newline)
  (display (lower-bound x))
  (display " to ")
  (display (upper-bound x)))

(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (subtract-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4) 
                 (max p1 p2 p3 p4))))

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

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
    (error "Divide by interval spanning zero" y))
  (mul-interval x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-center-width c (* (/ p 100) c)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* (/ (- (center i) (lower-bound i)) (center i)) 100))

; Test
(define int1 (make-interval 1 2))
(define int2 (make-interval 0.8 1.2))
(print-interval int1)
(print-interval int2)
(print-interval (add-interval int1 int2))
(print-interval (mul-interval int1 int2))
(print-interval (div-interval int2 int1))
(println "Test")
(print-interval (subtract-interval int1 int1))

(print-interval (mul-interval (make-interval 1 2) (make-interval 2 4))) ; = 2 to 8, width 0.5 * width 1 => width 6
(print-interval (mul-interval (make-interval 2 3) (make-interval 3 5))) ; = 6 to 15, width 0.5 * width 1 => width 4.5

; Test divide by interval spanning zero
(print-interval (div-interval int1 (make-interval 1 2))) ; OK
; (print-interval (div-interval int1 (make-interval -1 1))) ; Error

(check (mul-interval int1 int2) => (make-interval 0.8 2.4))
(check (mul-interval (make-interval -2 -1) (make-interval 3 5)) => (make-interval -10 -3))
(check (mul-interval (make-interval -1 2) (make-interval 3 5)) => (make-interval -5 10))
(check (mul-interval (make-interval 1 2) (make-interval 3 5)) => (make-interval 3 10))
(check (mul-interval (make-interval -2 -1) (make-interval -5 -3)) => (make-interval 3 10))
(check (mul-interval (make-interval -1 2) (make-interval -5 -3)) => (make-interval -10 5))
(check (mul-interval (make-interval 1 2) (make-interval -5 -3)) => (make-interval -10 -3))
(check (mul-interval (make-interval -2 -1) (make-interval -3 5)) => (make-interval -10 6))
(check (mul-interval (make-interval -1 2) (make-interval -3 5)) => (make-interval -6 10))
(check (mul-interval (make-interval -7 2) (make-interval -3 5)) => (make-interval -35 21))
(check (mul-interval (make-interval 1 2) (make-interval -3 5)) => (make-interval -6 10))

; Exercise 2.12
(check (make-center-percent 0 0) => (make-interval 0 0))
(check (make-center-percent 1 0) => (make-interval 1 1))
(check (make-center-percent 1 100) => (make-interval 0 2))
(check (make-center-percent 10 10) => (make-interval 9.0 11.0))
(check (center (make-center-percent 10 5)) => 10.0)
(check (percent (make-center-percent 10 5)) => 5.0)
