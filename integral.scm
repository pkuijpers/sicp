; Approximate integral of function f between a and b with accuracy n 
; using Simpson's rule
(define (integral f a b n)
  (define (h)
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k (h)))))
  (define (factor k)
    (cond ((= k 0) 1)
      ((= k n) 1)
      ((even? k) 2)
      (else 4)))
  (define (sum k)
    (cond ((= k 0) (y 0))
      (else
        (+ (* (factor k) (y k)) (sum (- k 1)))))
      )
  (* (/ (h) 3) (sum n))
)
  
(define (identity x) x)

(define (cube x) (* x x x))

(define (square x) (* x x))
    
(display (integral cube 0 1 100))
