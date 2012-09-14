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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))
  
(define (start-prime-test n start-time)
  (if (prime? n)
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
    
(search-for-primes 1000000000 1000000200)
