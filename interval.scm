; SICP 2.1.4: Interval arithmetic
(define (println line) (display line) (newline))

(define (make-interval a b) (cons a b))

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

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4) 
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

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
