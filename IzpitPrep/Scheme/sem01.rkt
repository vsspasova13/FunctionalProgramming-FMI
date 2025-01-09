#lang racket

(define (factorial n)
  (if (= n 0) 1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (cond ((= n 1) 0)
        ((= n 2) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (sum-interval a b)
  (if (> a b) 0
      (+ a (sum-interval (+ a 1) b))))

(define (count-digits n)
  (if (< n 10) 1
      (+ 1 (count-digits (quotient n 10)))))

(define (reverse-digits n)
  (define (helper acc n)
    (if (= n 0) acc
      (helper (+ (* acc 10) (remainder n 10)) (quotient n 10))))
  (helper 0 n))


(define (sum-list lst)
  (if (null? lst) 0
      (+ (car lst) (sum-list (cdr lst)))))