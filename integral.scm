(define (println line)
  (display line)
  (display "\n"))

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
  
(println (product-rec identity 1 inc 2)); 1 * 2 = 2
(println (product-rec identity 1 inc 3)); 1 * 2 * 3 = 6
(println (product-rec identity 1 inc 4)); 1 * 2 * 3 * 4 = 24

(define (factorial n)
  (product identity 1 inc n))
  
(println (factorial 4)); 4! = 24
(println (factorial 5)); 5! = 120
(println (factorial 10)); 10! = 3628800

(define (pi-approx n)
  (define (term t)
    (if (even? t)
       (/ t (+ t 1))
       (/ (+ t 1) t)))
  (* 4 (product term 2 inc n)))
  
(println (pi-approx 10));3.00217595455691
(println (pi-approx 100));3.12607890021541
(println (pi-approx 1000));3.14002381860059
(println (pi-approx 10000));3.14143559358986
(println (pi-approx 100000));3.14157694582264

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) 
        (combiner 
          (if (filter a) (term a) null-value) 
          result))))
  (iter a null-value))
  
(define (next n)
  (if (= n 2) 3 (+ n 2)))
  
(define (divides? a b)
  (= (remainder b a) 0))
  
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))
  
(define (prime? n)
  (= n (smallest-divisor n)))
  
(define (sum-square-primes a b)
  (filtered-accumulate + prime? 0 square a inc b))

(println (sum-square-primes 0 0)) ; 0
(println (sum-square-primes 0 1)) ; 1
(println (sum-square-primes 0 2)) ; 5
(println (sum-square-primes 0 10)) ; 1 2 3 5 7 = 1+4+9+25+49 = 88
(println (sum-square-primes 0 100)) ; 1 2 3 5 7 = 1+4+9+25+49 = 88

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
        
(define (product-relative-primes n)
  (define (relative-prime? a b)
    (= 1 (gcd a b)))
  (define (filter i)
    (relative-prime? i n))
  (if (< n 2) 0
    (filtered-accumulate * filter 1 identity 1 inc (- n 1))))

(println (product-relative-primes 3)); 2
(println (product-relative-primes 4)); 1 3 = 3
(println (product-relative-primes 5)); 1 2 3 4 = 24
(println (product-relative-primes 6)); 1 5 = 5
(println (product-relative-primes 7)); 1 2 3 4 5 6 7 = 720
