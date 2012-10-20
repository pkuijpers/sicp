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

(define (inc n) (+ n 1))
  
(define (identity x) x)

(define (cube x) (* x x x))

(define (square x) (* x x))
    
;(display (integral cube 0 1 100))
;(display "\n")

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))
  
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
  ;(define (iter a result)
  ;  (if (> a b)
  ;    result
  ;    (iter (next a) (+ (term a) result))))
  ;(iter a 0))
  
(define (sum-cubes a b)
  (sum cube a inc b))
  
(display (sum-cubes 1 10))(display '/n') ; 3025



(define (product term a next b)
  (accumulate * 1 term a next b))
  ;(define (iter a result)
  ;  (if (> a b)
  ;    result
  ;    (iter (next a) (* (term a) result))))
  ;(iter a 1))

  
(define (product-rec term a next b)
  (accumulate-rec * 1 term a next b))
  ;(if (> a b)
  ;  1
  ;  (* (term a) (product-rec term (next a) next b))))
  
(display (product-rec identity 1 inc 2))(display "\n"); 1 * 2 = 2
(display (product-rec identity 1 inc 3))(display "\n"); 1 * 2 * 3 = 6
(display (product-rec identity 1 inc 4))(display "\n"); 1 * 2 * 3 * 4 = 24

(define (factorial n)
  (product identity 1 inc n))
  
(display (factorial 4))(display "\n"); 4! = 24
(display (factorial 5))(display "\n"); 5! = 120
(display (factorial 10))(display "\n"); 10! = 3628800

(define (pi-approx n)
  (define (term t)
    (if (even? t)
       (/ t (+ t 1))
       (/ (+ t 1) t)))
  (* 4 (product term 2 inc n)))
  
(display (pi-approx 10))(display "\n");3.00217595455691
(display (pi-approx 100))(display "\n");3.12607890021541
(display (pi-approx 1000))(display "\n");3.14002381860059
(display (pi-approx 10000))(display "\n");3.14143559358986
(display (pi-approx 100000))(display "\n");3.14157694582264

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))
