#lang racket

(define (1+) (lambda (x) (+ x 1)))
(define (id x) x)

(define (foldr op init l)
  (if (null? l) init
      (op (car l) (foldr op init (cdr l)))))

(define (foldl op init l)
  (if (null? l) init
      (foldl op (op init (car l)) (cdr l))))

(define (sum l)
  (foldr + 0 l))

(define (len l)
  (foldr (lambda (x acc) (+ 1 acc)) 0 l))

(define (any? p l)
  (foldr (lambda (x acc) (or (p x) acc)) false l))

(define (all? p l)
  (foldr (lambda (x acc) (and (p x) acc)) true l))

(define (foldr1 op l)
  (foldr op (car l) (cdr l)))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define (minimum l)
  (foldr (lambda (x y) (if(< x y) x y)) (car l) (cdr l)))

(define (maximum l)
  (foldr (lambda (x y) (if(> x y) x y)) (car l) (cdr l)))

(define (map-foldr f l)
  (foldr (lambda (x xs) (cons (f x) xs)) '() l))

(define (filter-foldr p l)
  (foldr (lambda (x xs) (if (p x) (cons x xs) xs)) '() l))

(define (reverse-foldl l)
  (foldl (lambda (x xs) (cons xs x)) '() l))

(define (take n l)
  (if (or (= n 0) (null? l)) '()
      (cons (car l) (take (- n 1) (cdr l)))))

(define (drop n l)
  (cond ((null? l) '())
        ((= n 0) l)
        (else (drop (- n 1) (cdr l)))))

(define (take-while p? l)
  (cond ((null? l) '())
        ((p? (car l)) (cons (car l) (take-while p? (cdr l))))
        (else '())))

(define (drop-while p? l)
  (cond ((null? l) '())
        ((p? (car l)) (drop-while p? (cdr l)))
        (else l)))

(define (zip-with f xs ys)
  (cond ((or (null? xs) (null? ys)) '())
        (else (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys))))))

(define (zip xs ys)
  (zip-with cons xs ys))