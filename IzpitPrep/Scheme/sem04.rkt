#lang racket

(define (id x) x)
(define (1+) (lambda (x) (+ x 1)))
(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (len l)
  (if (null? l) 0
      (+ 1 (len (cdr l)))))

(define (minimum l)
  (define (helper min lst)
    (cond ((null? lst) min)
          ((< (car lst) min) (helper (car lst) (cdr lst)))
          (else (helper min (cdr lst)))))
  (helper (car l) (cdr l)))

(define (any? p l)
  (cond ((null? l) #f)
        ((p (car l)) #t)
        (else (any? p (cdr l)))))

;(define (all? p l))

(define (member? x l)
  (cond ((null? l) #f)
        ((equal? (car l) x) #t)
        (else (member? x (cdr l)))))

(define (at n l)
  (cond ((null? l) #f)
        ((= n 0) (car l))
        (else (at (- n 1) (cdr l)))))

(define (push-back x l)
  (if (null? l) (list x)
      (cons (car l) (push-back x (cdr l)))))

(define (reverse2 l)
  (foldl cons '() l))

(define (insert x n l)
  (cond ((> n (len l)) (push-back x l))
        ((= n 0) (cons x l))
        (else (cons (car l) (insert x (- n 1) (cdr l))))))

(define (range a b)
  (if (> a b) '()
      (cons a (range (+ a 1) b))))

(define (range-acc a b)
  (accumulate cons '() a b id (1+)))

(define (map f l)
  (if (null? l) '()
      (cons (f (car l)) (map f (cdr l)))))

(define (filter p l)
  (cond ((null? l) '())
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (reduce op init l)
  (if (null? l) init
      (op (car l) (reduce op init (cdr l)))))

(define (map-red f l)
  (reduce (lambda (x l) (cons (f x) l)) '() l))

(define (filter-red p l)
  (reduce (lambda (x l) (if (p x) (cons x l) l)) '() l))