#lang racket

(define (delit n)
  (define (iter i l)
    (cond ((= i 0) l)
          ((= (remainder n i) 0) (iter (- i 1) (cons i l)))
          (else (iter (- i 1) l))))
  (iter (- n 1) '()))
       
(define (done? n)
  (define sum (apply + (delit n)))
  (= (+ n 2) sum))

(define (any p? lst)
  (cond ((null? lst) #f)
        ((p? (car lst)) #t)
        (else (any p? (cdr lst)))))

(define (sum-almost-done a b)
  (let
      (
       [done-lst (filter done? (range a b))]
       )
    (foldr + 0
           (filter (lambda (x)
                     (any (lambda (curr-done)
                            (< (abs (- x curr-done)) (min (abs (- x a)) (abs (- b x)))))
                          done-lst))
                   (range a (+ b 1))))))

(define (operations op count l)
  (if (or (= 0 count) (null? (car l)) (null? (cadr l)) (symbol? (car l)) (symbol? (cadr l))) l
      (operations op (- count 1) (cons (op (car l) (cadr l)) (cddr l)))))

(define (stek l)
  (define (helper l res)
    (cond ((null? l) res)
          ((or (symbol? (car l)) (number? (car l))) (helper (cdr l) (cons (car l) res)))
          ((pair? (car l)) (helper (cdr l) (operations (caar l) (cdr (car l)) res)))
          (else (helper (cdr l) (map (lambda (x) (if (number? x) ((car l) x) x)) res)))))
  (helper l '()))
