(define (comp f g)
    (lambda (x)(f (g x))))

(define (k x) (+ x 1))

(define (iseven? x) (= (remainder x 2) 0))

(define (const c )
  (lambda (x) c))

(define (fmax f g)
  (lambda (x) (max (f x) (g x))))

(define (id x) (x))

(define relu (fmax id (const 0)))

(define (repeated n f x)
  (if (= n 0) x
       (repeated (- n 1) f (f x)))) 

(define (repeat n f)
  (lambda (x)
    (if (= n 0) x
       (repeated (- n 1) f (f x)))))

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))


(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (count? p? a b)
  (accumulate + 0 a b (lambda (x)(if (p? x) 1 0)) 1+))

(define (any? p? a b)
  (accumulate (lambda (a b) (or a b)) #f a b p? 1+))

(define (all? p? a b)
  (accumulate (lambda (a b) (and a b)) #t a b p? 1+))

(define (repeated2 n f x)
  (accumulate (lambda (a b)(f b)) x 1 n f 1+))

(define (repeat2 n f)
  (lambda (x) (accumulate (lambda (a b)(f b)) x 1 n f 1+)))


