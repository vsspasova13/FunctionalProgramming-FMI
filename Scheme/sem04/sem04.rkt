(define (len l)
  (if (null? l) 0
  (+ 1 (len (cdr l)))))

(define (min-list l)
  (define (helper minEl)
  (cond ((null? l) '())
        ((< (car l) minEl) (car l))
        (else (min-list (cdr l))))) (helper (car (cdr l))))

(define (any? p l)
  (cond ((null? l) #f)
        ((p (car l)) #t)
        (else (any? p (cdr l)))))

(define (all? p l)
  (cond ((null? l) #t)
        ((not(p (car l))) #f)
        (else (all? p (cdr l)))))

(define (member? x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) (cons (car l) (cdr l)))
        (else (member? x (cdr l)))))

(define (at n l)
  (cond ((null? l) #f)
        ((= n 0)(car l))
        (else (at (- n 1) (cdr l)))))

(define (push-back x l)
  (if (null? l) (list x)
      (cons (car l) (push-back x (cdr l)))))

(define (push-front x l)
  (if (null? l) (list x)
      (cons x l)))

(define (reversee l)
  (if (null? l) l
  (append (reverse (cdr l)) (list (car l)))))

(define (insert x n l)
  (cond ((null? l) (push-back x l))
        ((= n 1) (cons (car l) (push-front x (cdr l ))))
        (else (cons (car l) (insert x (- n 1) (cdr l))))))

(define (range a b)
  (if (> a b) '()
      (cons a (range (+ a 1) b))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (rangee a b)
  (accumulate cons '() a b (lambda (x) x) (lambda (x) (+ x 1))))

(define (mapp f l)
  (if (null? l) '()
      (cons (f (car l)) (mapp f (cdr l)))))

(define (filter p? l)
  (cond ((null? l) '())
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (reduce op init l)
  (if (null? l) init
      (op (car l) (reduce op init (cdr l)))))

