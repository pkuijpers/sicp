(define (println line) (display line) (newline))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (print-point p)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")")
	(newline))

(define (midpoint-segment s)
	(make-point (/ (- (x-point (end-segment s)) (x-point (start-segment s))) 2)
		(/ (- (y-point (end-segment s)) (y-point (start-segment s))) 2)))

; Test 2.2
(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 0 0)))) ; (0,0)
(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 0 2)))) ; (0,1)
(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 0)))) ; (1,0)
(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2)))) ; (1,1)

; Rectangles in a plane

; First representation
; (define (make-rect origin height width)
; 	(cons origin (cons height width)))
; (define (origin rect) (car rect))
; (define (height rect) (car (cdr rect)))
; (define (width rect) (cdr (cdr rect)))

; Second representation
(define (make-rect topleft bottomright)
	(cons topleft bottomright))
(define (origin rect) (car rect))
(define (height rect) (abs (- (y-point (cdr rect)) (y-point (car rect)))))
(define (width rect) (abs (- (x-point (cdr rect)) (x-point (car rect)))))

(define (perimeter rect)
	(+ (* 2 (height rect)) (* 2 (width rect))))
(define (area rect)
	(* (height rect) (width rect)))

; Test 2.3
; (define r1 (make-rect (make-point 0 0) 10 10))
(define r1 (make-rect (make-point 0 0) (make-point 10 10)))
(print-point (origin r1)) ; (0,0)
(println (height r1)) ; 10
(println (width r1)) ; 10
(println (perimeter r1)) ; 40
(println (area r1)) ; 100

; (define r2 (make-rect (make-point 2 3) 5 15))
(define r2 (make-rect (make-point 2 3) (make-point 17 8)))
(print-point (origin r2)) ; (2,3)
(println (height r2)) ; 5
(println (width r2)) ; 15
(println (perimeter r2)) ; 40
(println (area r2)) ; 75