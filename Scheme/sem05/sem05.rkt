#lang racket

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr op init l)
  (if (null? l)
      init
      (op (car l) (foldr op  init (cdr l)))))

(define (sum l)
  (foldr + 0 l))

(define (len l)
  (foldr (lambda (x i) (+ i 1)) 0 l))

(define (any? p l)
  (foldr (lambda (x y) (or (p x) y)) #f l))

(define (all? p l)
  (foldr (lambda (x y) (and (p x) y)) #t l))

(define (foldr1 op l)
  (foldr op (car l) (cdr l)))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define (minimum l)
  (foldr (lambda (x i) (if (< x i) x i)) (car l) l))

(define (maximum l)
  (foldr (lambda (x i) (if (> x i) x i)) (car l) l))

(define (map f l)
  (foldr (lambda (x) (f x)) '() l))

(define (filter? p l)
  (foldr (lambda (x y) ((p x) cons p y)) '() l))

(define (reverse l)
  (foldl (lambda (x y) (cons y x)) '() l))

(define (take n l)
  (cond((= n 0) '())
       ((null? l) '())
       (else (cons (car l) (take (- n 1) (cdr l))))))

(define (drop n l)
  (cond ((= n 0) l)
        ((null? l) '())
        (else (drop (- n 1) (cdr l)))))

(define (take-while p? l)
  (cond ((null? l) '())
         ((not(p? (car l))) '())
         (else (cons (car l) (take-while p? (cdr l))))))

(define (drop-while p? l)
  (cond ((null? l) '())
         ((p? (car l))(drop-while p? (cdr l)))
         (else l)))

(define (zip l1 l2)
  (cond ((null? l1) '())
        ((null? l2) '())
        (else (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2))))))

(define (zip-with op l1 l2)
  (cond ((null? l1) '())
        ((null? l2) '())
        (else (cons (op (car l1) (car l2)) (zip-with op (cdr l1) (cdr l2))))))