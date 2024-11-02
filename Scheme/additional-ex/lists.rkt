#lang racket


(define (increasing? l)
  (define (helper k l)
    (cond ((null? l) #t)
          ((> k (car l)) #f)
          (else (helper (car l) (cdr l)))))
  (helper (car l) (cdr l)))

(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

(define (progression? l)
  (let ((diff (- (cadr l) (car l))))
  (define (helper prev curr diff)
    (cond ((null? curr) #t)
          ((not(= (car curr) (+ prev diff))) #f)
          (else (helper (car curr) (cdr curr) diff))))
  (helper (car l) (cdr l) (- (cadr l) (car l)))))

(define (has-dublicates? l)
  (define (helper curr rem)
    (cond ((null? curr) #f)
          ((null? (cdr rem)) #f)
          ((member curr rem) #t)
          (else (helper (car rem) (cdr rem)))))
  (helper l (cdr l)))