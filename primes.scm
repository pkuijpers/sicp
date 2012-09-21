(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))
  
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))
    
(define (next n)
  (if (= n 2) 3 (+ n 2)))
    
(define (divides? a b)
  (= (remainder b a) 0))
  
(define (prime? n)
  (= n (smallest-divisor n)))
  
(define (expmod base exp m)
  (cond ((= exp 0) 1)
    ((even? exp)
      (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))
  
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))
  
(define (start-prime-test n start-time)
  ;(if (prime? n)
  (if (fast-prime? n 6)
    (report-prime (- (current-milliseconds) start-time))
    (report-smallest-divisor n)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
  
(define (report-smallest-divisor n)
  (display " - ")
  (display (smallest-divisor n)))
  
(define (search-for-primes start end)
  (cond ((<= start end)
    (timed-prime-test start)
    (search-for-primes (+ start 1) end))))
    
(define (fermat-test-full n)
    (define (fermat-test-full-it a)
        (cond ((= a 0) #t)
            ((= (expmod a n n) a) (fermat-test-full-it (- a 1)))
            (else #f)))
    (fermat-test-full-it (- n 1)))
    
;(search-for-primes 1000000000 1000000200)

(display (fermat-test-full 8))(display " = #f\n") ; non-primes do not pass
(display (fermat-test-full 3))(display " = #t\n") ; primes pass the test

; Carmichael numbers: not a prime, but pass the test
(display (fermat-test-full 561))(display " = #t\n")
(display (fermat-test-full 1105))(display " = #t\n")
(display (fermat-test-full 1729))(display " = #t\n")
(display (fermat-test-full 2465))(display " = #t\n")
(display (fermat-test-full 2821))(display " = #t\n")
(display (fermat-test-full 6601))(display " = #t\n")
