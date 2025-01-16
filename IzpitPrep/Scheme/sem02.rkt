#lang racket

(define (sum-digits n)
  (define (helper n sum)
    (if (= n 0) sum
        (helper (quotient n 10) (+ sum (remainder n 10)))))
  (helper n 0))

(define (count-divisors n)
  (define (helper n i count)
    (cond ((> i n) count)
          ((= (remainder n i) 0) (helper n (+ i 1) (+ count 1)))
          (else (helper n (+ i 1) count))))
  (helper n 1 0))

(define (prime? n)
  (define (prime-helper i n)
    (cond ((= i n) #t)
          ((= (remainder n i) 0) #f)
          (else (prime-helper (+ i 1) n))))
  (prime-helper 2 n))

(define (increasing-digits? n)
  (define (helper i k)
    (cond ((= k 0) #t)
          ((> (remainder k 10) i) #f)
          (else (helper (remainder k 10) (quotient k 10)))))
  (helper (remainder n 10) (quotient n 10)))

(define (ends-with? n k)
  (let ((nn (remainder n 10))
       (kk (remainder k 10)))
  (cond ((= k 0) #t)
        ((= n 0) #f)
        ((not (= kk nn)) #f)
        (else (ends-with? (quotient n 10) (quotient k 10))))))

(define (square x) (* x x))

(define (automorphic? n)
  (ends-with? (square n) n))

(define (perfect? n)
  (define (helper i n sum)
    (cond ((= i n) (= sum n))
          ((= (remainder n i) 0) (helper (+ i 1) n (+ sum i)))
          (else (helper (+ i 1) n sum))))
  (helper 1 n 0))

(define (pow n k)
  (define (helper i p)
    (if (= i k) p
        (helper (+ i 1) (* p n))))
  (cond ((= k 0) 1)
        ((< k 0) (/ 1 (helper 0 1)))
        (else (helper 0 1))))

(define (binary-to-decimal n)
  (define (helper k n acc)
    (if (= n 0) acc
        (helper (+ k 1) (quotient n 10) (+ acc (* (pow 2 k) (remainder n 10))))))
  (helper 0 n 0))

(define (decimal-to-binary n)
  (define (helper acc n)
    (if (= n 0) acc
        (helper (+ (* acc 10) (remainder n 2)) (quotient n 2))))
  (helper 0 n))
        