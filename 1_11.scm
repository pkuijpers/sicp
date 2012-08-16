#!/sw/bin/csi -ss
(define (f n)
   (cond ((< n 3) n)
         (else (+ (f (- n 1))
                  (* 2 (f (- n 2)))
                  (* 3 (f (- n 3)))))))
                  
(define (f-i n)
   (cond ((< n 3) n)
      (else (f-iter 0 1 2 (- n 2)))))
      
(define (f-iter f1 f2 f3 n)
   (cond ((= n 0) f3)
      (else (f-iter f2 f3 (+ (* 3 f1) (* 2 f2) f3) (- n 1)))))
      
(define number 100)
                  
(define (main args)
   (display (f-i number))
   (newline))
