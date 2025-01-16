#lang racket

(define (id x) x)

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (1+) (lambda (x) (+ x 1)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (const c)
  (lambda (x) c))

(define (fmax f g)
  (max (lambda (x) (f x)) (lambda (y) (g y))))

(define (repeated n f x)
    (if (= n 0) x
        (f (repeated (- n 1) f x))))

(define (repeat n f)
  (lambda (x) (repeated n f x)))

(define (count p? a b)
  (accumulate + 0 a b (lambda (x) (if (p? x) 1 0)) (1+)))

(define (any? p? a b)
  (accumulate (lambda (x y) (or x y)) #f a b p? (1+)))

(define (all? p? a b)
  (accumulate (lambda (x y) (and x y)) #f a b p? (1+)))

(define (repeat-acc n f)
  (accumulate compose id 1 n (const f) (1+)))

(define (repeated-acc n f x)
    (repeat-acc n f) x)

