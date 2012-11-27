(define (println line)
  (display line)
  (display "\n"))

(define tolerance 0.00001)

(define (square n)
  (* n n))

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (println guess)
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

(println "tan 100")  
(println (tan-cf 100 10))
(println (tan-cf 100 100))
(println (tan-cf 100 1000))
(println (tan-cf 100 10000))