#lang racket

(define (automorphic? n)
  (let ((s (square n)))
  (if (= n 0) #t
      (= (remainder s (exp 10 (length1 n))) (my-abs n)))))

(define (length1 n)
  (define (helper n acc)
    (if (= n 0) acc
        (helper (quotient n 10) (+ acc 1))))
  (helper n 0))

(define (my-abs n)
  (if (< n 0) (* (- 1) n)
      n))

(define (square n)
  (* n n))

(define (exp n k)
  (if (= k 0) 1
      (* n(exp n (- k 1)))))

(define (get-dist l1 l2)
  (sqrt (+ (square (- (car l1) (car l2)))
           (square (- (cdr l1) (cdr l2))))))

(define (shortest-distance points)
  (define (helper curr rem min rest)
    (cond ((or (null? curr)(null? rest)) min)
          ((null? rem) (helper (car rest) (cdr rest) min (cdr rest)))
          ((< (get-dist curr (car rem)) min) (helper curr (cdr rem) (get-dist curr (car rem)) rest))
          (else (helper curr (cdr rem) min rest))))
  (helper (car points) (cdr points) (get-dist (car points) (cadr points)) (cdr points)))