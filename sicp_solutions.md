Title: SICP Excercise Solutions
Author: Pieter Kuijpers

## Exercise 1.2 ##

    ( / (+ 5 4 (- 2 (+ 6 (/ 4 5)))) (* 3 (- 6 2) (- 2 7)))

## Exercise 1.3 ##

    (define (square x) (* x x))

    (define (first-smallest x y z)
      (and (<= x y) (<= x z))
    )

    (define (sum-square-two-largest x y z)
      (cond ((first-smallest x y z) (+ (square y) (square z)))
            ((first-smallest y x z) (+ (square x) (square z)))
            (else (+ (square x) (square y)))
      )
    )
    
## Exercise 1.4 ##

If b > 0 then a + b, else a - b  
So `a + |b|`

## Exercise 1.5 ##

Applicative order: `(p)` is evaluated first, resulting in an infinite loop.

Normal order: `(test 0 (p))` => `(if (= 0 0) 0 (p))` => `0`

## Exercise 1.6 ##

`new-if` is no special form, so its operands are evaluated first. The second
operand will be evaluated for each interation, resulting in an infinite loop.

## Exercise 1.7 ##

If `x` itself is very small, the tolerance is relatively large compared with
the number, resulting in a large relative error.

    (good-enough? 0.01 0.001) => true

For very large numbers, the precision of the tolerance is larger than the
precision of the number.

    (good-enough? (+ 3e6 0.0001) 9e12) => false
    
A better way: check that guess differs from previous-guess with at least
0.1% .

    (define (good-enough? guess previous-guess)
      (< (/ (abs (- previous-guess guess)) guess) 0.001))
      
    (define (sqrt-iter guess previous-guess x)
      (if (good-enough? guess previous-guess)
        guess
        (sqrt-iter (improve guess x)
          guess
          x)))
          
    (define (sqrt x)
      (sqrt-iter 1.0 2.0 x))
      
## Exercise 1.8 ##

Only the function for generating a new guess needs to be updated:

    (define (improve guess x)
      (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

## Exercise 1.9 ##

First case is recursive:

    (+ 4 5)
    (inc (+ 3 5))
    (inc (inc (+ 2 5)))
    (inc (inc (inc (+ 1 5))))
    (inc (inc (inc (inc (+ 0 5)))))
    (inc (inc (inc (inc 5))))
    (inc (inc (inc 6)))
    (inc (inc 7))
    (inc 8)
    9
    
Second case is iterative:

    (+ 4 5)
    (+ 3 6)
    (+ 2 7)
    (+ 1 8)
    (+ 0 9)
    9
    
## Exercise 1.10 ##

    (A 1 10)
    (A 0 (A 1 9))
    (A 0 (A 0 (A 1 8)))
    (A 0 (A 0 (A 0 (A 1 7))))
    (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))    
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
    (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
    (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
    (A 0 (A 0 (A 0 (A 0 64))))
    (A 0 (A 0 (A 0 128)))
    (A 0 (A 0 256))
    (A 0 512)
    1024
    2^10 = (2^10)^1
    
    (A 2 4)
    (A 1 (A 2 3))
    (A 1 (A 1 (A 2 2)))
    (A 1 (A 1 (A 1 (A 2 1))))
    (A 1 (A 1 (A 1 2)))
    (A 1 (A 1 (A 0 (A 1 1))))
    (A 1 (A 1 (A 0 2)))
    (A 1 (A 1 4))
    (A 1 (A 0 (A 1 3)))
    (A 1 (A 0 (A 0 (A 1 2))))
    (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
    (A 1 (A 0 (A 0 (A 0 2))))
    (A 1 (A 0 (A 0 4)))
    (A 1 (A 0 8))
    (A 1 16)
    ...
    2^16 = 2^(2^4)
    
    (A 3 3)
    (A 2 (A 3 2))
    (A 2 (A 2 (A 3 1)))
    (A 2 (A 2 2))
    ...
    (A 2 4)
    ...
    2^16 = (2^(2^(2^3))
    
    (A 0 n) = 2n
    (A 1 n) = 2^n
    (A 2 n) = 2^(2^(2^... n times
    
## Exercise 1.11 ##

Recursive:

    (define (f n) 
        (if (< n 3)
        n
        (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
        
Iterative:

    (define (f n)
        (define (f-iter a b c n)
            (if (< n 3)
                c
                (f-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))
            )
        )
        (if (< n 3)
            n
            (f-iter 0 1 2 n)))
        
## Exercise 1.12 ##

    (define (pascal r c)
        (cond ((< c 1) 0)
              ((> c r) 0)
              ((= r 1) 1)
              ((= r c) 1)
              (else (+ (pascal (- r 1) (- c 1))
                       (pascal (- r 1) c)))))
                       
## Exercise 1.13 ##

Too hard for me, see [someone else's solution] (http://www.billthelizard.com/2009/12/sicp-exercise-113-fibonacci-and-golden.html)

## Exercise 1.14 ##

![Process tree for (count-change 11)](1_14.svg)

## Exercise 1.15 ##

a. (p (sine (/ 12.15 3.0))) = (p (sine 4.05))  
   (p (sine (/ 4.05 3.0))) = (p (sine 1.35))  
   (p (sine (/ 1.35 3.0))) = (p (sine 0.45))  
   (p (sine (/ 0.45 3.0))) = (p (sine 0.15))  
   (p (sine (/ 0.15 3.0))) = (p (sine 0.05)) = (p 0.05)
   
   So p is applied 5 times

b. For each multiplication of x by 3, one additional step is added. So growth
order is logarithmic: Θ(log(n))

## Exercise 1.16 ##

    (define (fast-exp b n)
        (define (fast-exp-iter a b n)
            (cond ((= n 1) a)
                  ((even? n) (fast-exp-iter a (square b) (/ n 2)))
                  (else (fast-exp-iter (* a b) b (- n 1)))))
        (fast-exp-iter 1 b n))
        
## Exercise 1.17 ##

    (define (fast-* a b)
        (cond ((= b 0) 0)
              ((even? b) (fast-* (double a) (halve b)))
              (else (+ a (fast-* a (- b 1))))))
              
a * b = 2a * b/2 if even  
a + (a * (b-1)) if odd

Test:

    5 4
    10 2
    20 1
    20 + (20 0)
    20

    4 3
    4 + (4 2)
    4 + (8 1)
    4 + 8 + (8 0)
    12
    
## Exercise 1.18 ##

a b -> c d n
a * b -> c + (b * n)

    (define (fast-* a b)
        (define (fast-iter-* c d n)
            (cond ((= n 0) c)
                  ((even? n) (fast-iter-* c (double d) (halve n)))
                  (else (fast-iter-* (+ c d) d (- b n)))))
        (fast-iter-* 0 a b))
        
Test:

    5 4
    0 5 4
    0 10 2
    0 20 1
    20 20 0
    20
    
    4 3
    0 4 3
    4 4 2
    4 8 1
    12 8 0
    12
    
    2 7
    0 2 7
    2 2 6
    2 4 3
    6 4 2
    6 8 1
    14 8 0
    14
    
## Exercise 1.19 - 1.21 ##

Lost after a hard disk crash.

## Exercise 1.22 ##

    (define (search-for-primes start end)
      (cond ((<= start end)
        (timed-prime-test start)
        (search-for-primes (+ start 1) end))))

    1000003 *** 1.0
    1000033 *** 1.0
    1000037 *** 1.0

    10000019 *** 4.0
    10000079 *** 3.0
    10000103 *** 3.0

    100000007 *** 14.0
    100000037 *** 12.0
    100000039 *** 10.0

    1000000007 *** 34.0
    1000000009 *** 29.0
    1000000021 *** 30.0
    
Factor 10 increase in n results in a factor sqrt(10) ≈ 3.2 in running time.

## Exercise 1.23 ##

    (define (next n)
      (if (= n 2) 3 (+ n 2)))

    100000007 *** 7.0
    100000037 *** 6.0
    100000039 *** 6.0

About twice as fast - as expected.

    1000000007 *** 19.0
    1000000009 *** 19.0
    1000000021 *** 19.0
    
About 1.5 times as fast, lower than expected. An extra operation is needed to
determine `next`.

## Exercise 1.24 ##

O(log n): I would expect the running time to increase by a constant amount
after a 10-fold increase of n.

    (define (start-prime-test n start-time)
      (if (fast-prime? n 6)
        (report-prime (- (current-milliseconds) start-time))
        (report-smallest-divisor n)))
        
    1000003 *** 1.0
    1000033 *** 1.0
    1000037 *** 1.0
    
    10000019 *** 1.0
    10000079 *** 0.0
    10000103 *** 1.0
    
    100000007 *** 1.0
    100000037 *** 0.0
    100000039 *** 1.0
    
    1000000007 *** 1.0
    1000000009 *** 1.0
    1000000021 *** 1.0

Seems to run in about constant time: the constant amount by which the running
time is increased by a 10-fold increase in n is apparently too small to
measure.

## Exercise 1.25 ##

The difference is that in this approach `(fast-expt base exp)` is calculated
before the remainder is taken. In the original approach, the reduction steps
keep working on the remainder, so the numbers that are used in the remainder
operation are much smaller. The larger numbers in this approach could be a
problem for large values of `n`, where the exponent goes beyond the maximum
value of an integer.

## Exercise 1.26 ##

For each reduction step in `expmod`, `(expmod base (/ exp 2) m)` is now
executed *twice*. So the exponent is divided by two, but executed twice,
resulting in a complexity of O(n) instead of O(log n).

## Exercise 1.27 ##
    (define (fermat-test-full n)
        (define (fermat-test-full-it a)
            (cond ((= a 0) #t)
                ((= (expmod a n n) a) (fermat-test-full-it (- a 1)))
                (else #f)))
        (fermat-test-full-it (- n 1)))

All the given numbers pass the test, but have a smallest-divisor greater than
1.

## Exercise 1.28 ##

    ; #t if a is a 'nontrivial square root of 1 module n'
    (define (nontriv-sqrt-mod? a n)
      (and (not (= a 1))
          (not (= a -1))
          (= (remainder (square a) n) 1)))

    (define (expmod base exp m)
      (cond ((= exp 0) 1)
        ((nontriv-sqrt-mod? base m) 0)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

    (define (miller-rabin-test n)
      (define (try-it a)
        (= (expmod a (- n 1) n) 1))
      (try-it (+ 1 (random (- n 1)))))
      
`expmod` now returns 0 if `base` is a nontrivial square root of 1 modulo n,
signaling that n is not a prime. `fast-prime?` now correctly identifies primes
and non-primes, even the Carmichael numbers (if a large enough number of tries
is issued, 5 was usually enough).

## Exercise 1.29 ##

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
      (* (/ (h) 3) (sum n)))
      
## Exercise 1.30 ##

    (define (sum term a next b)
      (define (iter a result)
        (if (> a b)
          result
          (iter (next a) (+ (term a) result))))
      (iter a 0))
      
## Exercise 1.31 ##
### a. ###

    (define (product term a next b)
      (define (iter a result)
        (if (> a b)
          result
          (iter (next a) (* (term a) result))))
      (iter a 1))

    (define (factorial n)
        (product identity 1 inc n))
        
    (define (pi-approx n)
      (define (term t)
        (if (even? t)
           (/ t (+ t 1))
           (/ (+ t 1) t)))
      (* 4 (product term 2 inc n)))
      
NB: `pi-approx` doesn't seem to be very efficient: it needs about a 10-fold
increase in `n` to add 1 digit of significance to the result.

| n     | result           |
| -     | -                |
| 10    | 3.00217595455691 |
| 100   | 3.12607890021541 |
| 1000  | 3.14002381860059 |
| 10000 | 3.14143559358986 |
| 100000 | 3.14157694582264 |

### b. ###

    (define (product-rec term a next b)
      (if (> a b)
        1
        (* (term a) (product-rec term (next a) next b))))
        
## Exercise 1.32 ##
### a ###

    (define (accumulate combiner null-value term a next b)
      (define (iter a result)
        (if (> a b)
          result
          (iter (next a) (combiner (term a) result))))
      (iter a null-value))
      
    (define (sum term a next b)
      (accumulate + 0 term a next b))
      
    (define (product term a next b)
      (accumulate * 1 term a next b))
      
### b ###

    (define (accumulate-rec combiner null-value term a next b)
      (if (> a b)
        null-value
        (combiner 
            (term a) 
            (accumulate-rec combiner null-value term (next a) next b))))
            
## Exercise 1.33 ##

    (define (filtered-accumulate combiner filter null-value term a next b)
      (define (iter a result)
        (if (> a b)
          result
          (iter (next a) 
            (combiner 
              (if (filter a) (term a) null-value) 
              result))))
      (iter a null-value))
      
### a ###

    (define (sum-square-primes a b)
      (filtered-accumulate + prime? 0 square a inc b))
      
### b ###

    (define (product-relative-primes n)
      (define (relative-prime? a b)
        (= 1 (gcd a b)))
      (define (filter i)
        (relative-prime? i n))
      (if (< n 2) 0
        (filtered-accumulate * filter 1 identity 1 inc (- n 1))))
        
## Exercise 1.34 ##

    (f f)
    (f 2)
    (2 2)
=> Error, 2 is not a function

    The object 2 is not applicable.

## Exercise 1.35 ##

Golden ratio: phi^2 = phi + 1  
x^2 = x + 1 <=> x = 1 + 1/x  
Thus phi is the fixed point of x = 1 + 1/x

    (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0)
_1.61803278688525_

## Exercise 1.36 ##

Without average damping:
    
    (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0)
_4.55554091291796_  
Found after 34 iterations.

With average damping:
    
    (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)
_4.55554655214737_  
Found after 9 iterations.

Average damping makes the procedure converge much faster.

## Exercise 1.37 ##

### a ###

    (define (cont-frac n d k)
      (define (iterate i)
        (if (= i k)
          (/ (n i) (d i))
          (/ (n i) (+ (d i) (iterate (+ i 1))))))
      (iterate 1)
    )
    
    (println (cont-frac (lambda (i) 1.0) (lambda(i) 1.0) k))
    
Actual value is 0.6180339887498948482...
    
| k     | result            |
| -     | -                 |
| 1     | 1.0               |
| 2     | 0.5               |
| 3     | 0.666666666666667 |
| 5     | 0.625             |
| 8     | 0.617647058823529 |
| 11    | 0.618055555555556 |

### b ###

The procedure from a generates a recursive process.

Iterative process:

    (define (cont-frac-iter n d k)
      (define (iterate i result)
        (if (zero? i)
          result
          (iterate (- i 1) (/ (n i) (+ (d i) result)))))
      (iterate k 0))
      
## Exercise 1.38 ##

The tricky part is determining the value of d for i:

    (define (e-approx n)
      (define (d i)
        (if (zero? (remainder (+ i 1) 3))
          (* (/ (+ i 1) 3) 2)
          1))
      (+ 2 (cont-frac-iter (lambda (i) 1.0) d n)))

    (e-approx 100)
_2.71828182845905_

## Exercise 1.39 ##

    (define (tan-cf x k)
      (define (n i)
        (if (= i 1) x (* (square x) -1)))
      (define (d i)
        (- (* i 2) 1))
      (cont-frac-iter n d k))
      
Convergence is a bit slow:

tan(100) actual value is -0.587213915156929

| k     | result             |
| -     | -                  |
| 10    | -0.613762972647002 |
| 100   | -1.46895929637298  |
| 1000  | -0.587213915156929 |
| 10000 | -0.587213915156929 |

## Exercise 1.40 ##

`(cubic a b c)` returns a procedure `f` so that `(f x) = x^3 + ax^2 + bx + c`

    (define (cubic a b c)
      (lambda (x)
        (+ (cube x) (* a (square x)) (* b x) c)))

## Exercise 1.41 ##

    (define (double f)
      (lambda (x)
        (f (f x))))
        
    ((double inc) 5) => 5 + 2 = 6
    (((double double) inc) 5) => 5 + 2^2 = 9
    (((double (double double)) inc) 5) => 5 + 4^2 = 21
    
## Exercise 1.42 ##

    (define (compose f g)
      (lambda (x)
        (f (g x))))
        
## Exercise 1.43 ##

    (define (repeated f n)
      (if (= n 1) f
        (compose f (repeated f (- n 1)))))

## Exercise 1.44 ##

    (define (smooth f)
      (lambda (x)
        (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

    ; n-fold smoothed function f
    (((repeated smooth 2) sin) 1)

## Exercise 1.45 ##

Number of repeated average damping needed for converging:

| n-th root | number of average damping |
| -         | -                         |
| 4         | 2                         |
| 8         | 3                         |
| 31        | 3                         |
| 32        | 3                         |
| 67        | 3                         |
| 68        | 4                         |
| 128       | 4                         |
| 200       | 5                         |

It's some logarithmic scale, not quite sure what it is. 
[Some solutions](http://community.schemewiki.org/?sicp-ex-1.45) suggest using floor(log2(n)).

    (define (log2 x) (/ (log x) (log 2)))

    (define (nth-root x n)
      (fixed-point 
        ((repeated average-damp (floor (log2 n))) 
          (lambda (y) (/ x (exp y (- n 1)))))
        1.0))

## Exercise 1.46 ##

    (define (iterative-improve good-enough? improve)
      (lambda (guess)
        (if (good-enough? guess) guess
          ((iterative-improve good-enough? improve) (improve guess)))))

    (define (sqrt x)
      ((iterative-improve
        (lambda (guess)
          (< (abs (- (square guess) x)) 0.001))
        (lambda (guess)
          (average guess (/ x guess)))
        ) 1.0))

    (define (fixed-point-new f first-guess)
      ((iterative-improve
        ; good-enough?
        (lambda (guess)
          (let ((next (f guess)))
            (< (abs (- guess next)) tolerance)))
        ; improve
        (lambda (guess)
          (f guess)))
      first-guess))
