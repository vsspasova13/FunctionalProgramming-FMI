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

(define (dedup l)
  (define (helper curr rem)
    (cond ((null? rem) '())
          ((member (car rem) curr) (helper curr (cdr rem)))
          (else (cons (car rem) (helper (cons (car rem) curr) (cdr rem))))))
  (helper '() l))

(define (dedup2 l)
  (foldr (lambda (x acc)
         (if (member x acc) acc
             (cons x acc)))
  '() l))

(define (union l s)
  (append l (filter (lambda (x) (not (member x l))) s)))

(define (intersection l s)
  (append '() (filter (lambda (x) (and (member x l) (member x s))) s)))

(define (product l s)
  (define (helper x p acc)
  (cond ((null? x) acc)
        ((null? p) (helper (cdr x) s acc))
        (else (helper x (cdr p) (append acc (list (cons (car x) (car p)) ))))))
  (helper l s '()))

(define (product2 l s)
  (apply append
         (map (lambda (x) ( map (lambda (y) (cons x y)) s)) l)))

(define (scalar-product xs ys)
  (define (helper l s acc)
          (if (null? s) acc
              (helper (cdr l) (cdr s) (+ (* (car l) (car s)) acc))))
  (helper xs ys 0))

(define (list-ind i l)
  (if (null? l)'()
      (if (= i 0) (car l)
          (list-ind (- i 1) (cdr l)))))

(define (main-diagonal xs)
  (define (iter i xs)
    (if (null? xs) '()
        (cons (list-ind i (car xs)) (iter (+ i 1) (cdr xs)))))
  (iter 0 xs))
  
(define (second-diagonal xs)
  (define (iter i xs)
    (if (null? xs) '()
        (cons (list-ind i (car xs)) (iter (- i 1) (cdr xs)))))
  (iter (- (length xs) 1) xs))

(define (diagonal-product matrix)
  (scalar-product (main-diagonal matrix) (second-diagonal matrix)))