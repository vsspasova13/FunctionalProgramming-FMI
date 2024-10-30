(define (1+ x) (+ x 1))
(define (id x) x)

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1))x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0) id
      (compose f (repeated f (- n 1)))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (repeated f n)
  (accumulate compose id 1 n (lambda (x) f) 1+))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n f n dx)
  (if (= n 0) f
      (derive (derive-n f (- n 1) dx) dx)))

(define (derive-n f n dx)
  ((repeated (lambda (f) (derive f dx)) n) f))

(define (derive-n f n dx)
  ((accumulate compose id 1 n
               (lambda (i) (lambda (f) (derive f dx))) 1+)f))