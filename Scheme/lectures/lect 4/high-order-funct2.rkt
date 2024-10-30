#lang racket

(define (1+ x) (+ x 1))
(define (id x) x)

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1))x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated1 f n)
  (if (= n 0) id
      (compose f (repeated1 f (- n 1)))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (repeated2 f n)
  (accumulate compose id 1 n (lambda (x) f) 1+))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n f n dx)
  (if (= n 0) f
      (derive (derive-n f (- n 1) dx) dx)))

(define (derive-n2 f n dx)
  ((repeated (lambda (f) (derive f dx)) n) f))

(define (derive-n3 f n dx)
  ((accumulate compose id 1 n
               (lambda (i) (lambda (f) (derive f dx))) 1+)f))

(define (exists? p? l)
  (cond ((null? l) #f)
        ((p? (car l)) #t)
        (else (exists? p? (cdr l)))))

(define (exists?2 p? l)
  (and (not (null? l)) (or (p? (car l)) (exists? p? (cdr l)))))

(define (member? x l)
  (exists? (lambda (y) (equal? x y)) l))

(define (search p? l)
  (and (not (null? l)) (or (and (p? (car l)) l) (search p? (cdr l)))))

(define (member x l)
  (search (lambda (y) (equal? x y)) l))

(define (all? p? l)
  (not (search (lambda (x) (not (p? x))) l)))