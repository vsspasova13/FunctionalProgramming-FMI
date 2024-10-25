(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (id x) x)
(define (1+ x) (+ x 1))

(define (integral a b f dx)
  (* dx (accumulate  + 0 a b (lambda (x) (+ x dx)))))

(define (fact n)
  (accumulate * 1 1 n (lambda (i) i) 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (my-exp x n)
  (accumulate + 0 0 n (lambda (i) (/ (pow x i) (fact i))) 1+))

(define (my-exp x n)
  (accumulate + 0 0 n (lambda (i) (/ (accumulate * 1 1 i (lambda (i) x) 1+)
                                     (accumulate * 1 1 i (lambda (i) i) 1+))) 1+))

(define (exists? a b p?)
  (accumulate (lambda (x y) (or x y)) #f a b p? 1+))