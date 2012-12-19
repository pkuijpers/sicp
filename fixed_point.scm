(define (println line)
  (display line)
  (display "\n"))

(define tolerance 0.00001)

(define (square n)
  (* n n))
  
(define (cube n)
  (* n n n))

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
    (if (close-enough? guess next)
      next
      (try next))))
  (try first-guess))

;(println (fixed-point cos 1.0))
;(println (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))
;(println (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))
;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

(define (cont-frac n d k)
  (define (iterate i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (iterate (+ i 1))))))
  (iterate 1))

(println (cont-frac (lambda (i) 1.0) (lambda(i) 1.0) 11))

(define (cont-frac-iter n d k)
  (define (iterate i result)
    (if (zero? i)
      result
      (iterate (- i 1) (/ (n i) (+ (d i) result)))))
  (iterate k 0))
  
(println (cont-frac-iter (lambda (i) 1.0) (lambda(i) 1.0) 11))

(define (e-approx n)
  (define (d i)
    (if (zero? (remainder (+ i 1) 3))
      (* (/ (+ i 1) 3) 2)
      1))
  (+ 2 (cont-frac-iter (lambda (i) 1.0) d n)))
    
;(println (e-approx 1000))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1) x (* (square x) -1)))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac-iter n d k))

;(println "tan 100")  
;(println (tan-cf 100 10))
;(println (tan-cf 100 100))
;(println (tan-cf 100 1000))
;(println (tan-cf 100 10000))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
      dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))
    
(println ((cubic 1 1 1) 2)) ; 8 + 4 + 2 + 1 = 15
(println ((cubic 1 2 3) 2)) ; 8 + 4 + 4 + 3 = 19

(println "Root of (cubic 1 1 1)")
(println (newtons-method (cubic 1 1 1) 1)) ; x^3 + x^2 + x + 1 = 0 <=> x = -1

(println "Root of (cubic 1 2 3)")
(println (newtons-method (cubic 1 2 3) 1)) ; x^3 + x^2 + 2x + 3 = 0 <=> x = -1.27568220364985

(define (inc i)
  (+ i 1))

(define (double f)
  (lambda (x)
    (f (f x))))
    
(println ((double inc) 4))

; (println (((double double) inc) 5))  ; 5 + 4 = 9
; (println (((double (double double)) inc) 5))  ; 5 + 8 = 13

(define (compose f g)
  (lambda (x)
    (f (g x))))
    
(println ((compose square inc) 6))

(define (repeated f n)
  (if (= n 1) f
    (compose f (repeated f (- n 1)))))

; (println ((repeated square 1) 5)); 25    
; (println ((repeated square 2) 5)); 625

(define (smooth f)
  (lambda (x)
    (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(println (sin 1))
(println ((smooth sin) 1))

; n-fold smoothed function f
(((repeated smooth 2) sin) 1)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (exp x n)
  (if (zero? n) 1
    (* x (exp x (- n 1)))))

(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (cube y)))) 1.0))

(define (fifth-root x)
  (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (* y y y y)))) 1.0))

(define (log2 x) (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point 
    ((repeated average-damp (floor (log2 n))) 
      (lambda (y) (/ x (exp y (- n 1)))))
    1.0))

(println (fourth-root 3.0))
(println (nth-root 3.0 4))

(println (fifth-root 3.0))
(println (nth-root 3.0 5))

(println (nth-root 3.0 200))

