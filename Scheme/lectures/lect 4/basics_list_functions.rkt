(define (lengthh l)
  (if (null? l) 0
      (+ 1 (lengthh (cdr l)))))

(define (lengthhh l)
  (foldrr (lambda (x r) (+ r 1)) 0 l))

(define (appendd l1 l2)
  (if (null? l1) l2
      (cons (car l1) (appendd (cdr l1) l2))))

(define (appenddd l1 l2)
  (foldrr cons l2 l1))

(define (snocc x l)
  (appendd l (list x)))

(define (reversee l)
  (if (null? l) l
      (appendd (reversee (cdr l)) (list (car l)))))

(define (reversee l)
  (if (null? l) l
      (snocc (car l) (reversee (cdr l)))))

(define (reverseee l)
  (foldrr snocc '() l))

(define (reverssee l)
  (foldrr (lambda (x y) (cons y x)) '() l))

(define (list-taill l n)
  (if( = n 0) l
  (list-taill (cdr l) (- n 1))))

(define (list-reff l n)
  (if (= n 0) (car 1)
      (list-reff (cdr l) (- n 1))))

(define (memberr x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) l)
        (else (memberr x (cdr l)))))

(define (member* =? x l)
  (cond ((null? l) #f)
  ((=? x (car l)) l)
  (else (member* =? x (cdr l)))))

(define (member* =? x l)
  (and (not (null? l)) (or (and (=? x (car l)) l) (member* =? x (cdr l)))))

(define (memberr x l) (member* equal? x l))
(define (memvv x l) (member* eqv? x l))
(define (memqq x l) (member* eq? x 1))

(define (from-too a b)
  (if (> a b) '()
      (cons a (from-too (+ a 1) b))))

(define (collectt a b next)
  (if (> a b) '()
      (cons a (collectt (next a) b next))))

(define 1+ (lambda (x) (+ x 1)))

(define (from-too a b)
  (collectt a b 1+))

(define (filterr p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filterr p? (cdr l))))
        (else (filterr p? (cdr l)))))

(define (mapp f l)
  (if (null? l) l
      (cons (f (car l)) (mapp f (cdr l)))))

(define (maap f l)
  (foldr (lambda (x r) (cons (f x) r)) '() l)) 

(define (foldrr op nv l)
  (if (null? l) nv
  (op (car l) (foldrr op nv (cdr l)))))

