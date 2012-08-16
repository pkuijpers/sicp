#!/sw/bin/csi -ss

(define (cube x) (* x x x))

;;; Summation of a series
(define (sum term a next b)
   (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;; Numerical integration using Simpson's rule
(define (simpson-sum f a b i n)
   (define h
      (/ (- b a) n))
   (define (simpson-term x)
      (* (/ h 3)
         (f (+ a (* x h)))))
   (define (inc n) (+ n 1))
   (sum simpson-term i inc n))

(define (main args)
   (display (simpson-sum cube 0 1 0 100))
   (newline))