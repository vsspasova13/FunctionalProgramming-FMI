#lang racket

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (reverse n)
  (define (helper x res)
  (if (= x 0) res
      (helper (quotient x 10) (+ (* res 10)(remainder x 10)))))
  (helper n 0))

(define (palindrome? n)
 (= n (reverse n)))

(define (count-palindromes a b)
  (define (helper a b count)
    (cond ((> a b) count)
          ((palindrome? a) (helper (+ a 1) b (+ count 1)))
          (else (helper (+ a 1) b count))))
  (helper a b 0))

(define (isPrime? n)
  (define (helper n i)
  (cond ((= i n) #t)
        ((= (remainder n i) 0) #f)
        (else (helper n (+ i 1)))))
   (helper n 2 )) 

(define (sum-primes n k)
  (define (helper acc i curr)
    (cond ((= i k) acc)
          ((isPrime? curr) (helper (+ acc curr) (+ i 1) (+ curr 1)))
          (else (helper acc i (+ curr 1)))))
  (helper 0 0 (+ n 1)))
          
(define (prime-factors n)
  (define (helper i n k l)
    (cond ((= 1 n)
           (if (> k 0) (cons (cons i k) l) l))
          ((and (isPrime? i) (= (remainder n i) 0)) (helper i (quotient n i) (+ k 1) l))
          ((> k 0) (helper (+ i 1) n 0 (cons (cons i k) l)))
          (else (helper (+ i 1) n 0 l))))
  (helper 2 n 0 `()))
 
        