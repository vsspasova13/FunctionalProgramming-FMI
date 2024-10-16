
(define (sum-digits-wrapper n)
  (define (sum-digits n sum)
    ( if(< n 10) sum
        (sum-digits (quotient n 10) (+ sum (remainder n 10)))))
  (sum-digits n 0))

(define (count-divisors n)
  (define (helper i n br)
    (if (> i n)
       br
        (if (=(remainder n i) 0)
            (helper (+ i 1)
                    n
                    (+ br 1))
         (helper (+ i 1)
                 n
                 br))))
 (helper 1 n 0))

(define (prime? n)
  (define (helper i n)
   (cond
    ((= i n) #t)
    ((= (remainder n i) 0) #f)
    (else (helper (+ i 1) n))))
  (helper 2 n))

(define (increasing-digits? n)
  (define (helper curr n)
    (cond
      ((= n 0) #t)
      ((> (remainder n 10) curr) #f)
    (else (helper (remainder n 10) (quotient n 10)))))
 ( helper (quotient n 10) n))

(define (ends-with? n k)
  (cond
    ((< k 10) (= (remainder n 10) k))
    ((not(= (remainder n 10) (remainder k 10))) #f)
    (else (ends-with? (quotient n 10) (quotient k 10)))))

(define (automorphic? n)
  (ends-with? (* n n) n))

(define (perfect? n)
  (define (helper i n sum)
  (cond
    ((= i 1) (= sum n))
    ((= (remainder n i) 0) (helper (- i 1) n (+ sum i)))
    (else (helper (- i 1) n sum))))
  (helper(- n 1) n 0))
    