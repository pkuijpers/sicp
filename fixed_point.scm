(define (println line)
  (display line)
  (display "\n"))

(define tolerance 0.00001)

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
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
