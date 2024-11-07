#lang racket

(define m '((1 2 3) (4 5 6)))

(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))
       (all? list? m)
       (all? (lambda (row) (= (length row)
                              (length (car m)))) m)))

(define (get-rows m) (length m))
(define (get-colls m) (length (car m)))

(define (get-first-row m) (car m))
(define (get-first-coll m) (map car m))

(define (del-first-row m) (cdr m))
(define (del-first-coll m) (map cdr m))

(define (get-row i m) (list-ref m i))
(define (get-coll i m) (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (if (null? (get-first-row m)) '()
      (cons (get-first-coll m)(transpose (del-first-coll m)))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))

(define (transpose2 m)
  (accumulate cons '() 0 (- (get-colls m) 1) (lambda (i) (get-coll i m)) 1+))

(define (transpose3 m)
  (apply map list m))

(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrix m1 m2) (map sum-vectors m1 m2))

(define (dot-product v1 v2) (apply + 0 (map * v1 v2)))

(define (multiply-matrixes m1 m2)
  (map (lambda (row) (map (lambda (coll) (dot-product row coll)) (transpose m2)) ) m1))

