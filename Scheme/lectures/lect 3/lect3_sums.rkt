(define (sum1 k)
  (if (> k 100) 0
      (+ (* k k) (sum1 (+ k 1)))))

(define (sum2 a b dx f)
  (if (> a b) 0
      (+ (* dx (f a)) (sum2 (+ a dx) b dx f))))

(define (sum3 x)
  (if (> x (expr 10 1000)) 0
      (+ x (sum3 (exp x)))))

(define (sum a b term next)
  (if (> a b) 0
      (+ (term a) (sum (next a) b term next))))

(define (sqr x)(* x x))
(define (1+ x)(+ x 1))

(define (sum1-new k) (sum k 100 sqr 1+))

(define (sum2-new a b dx f)
  ;(define (term x)(* dx (f x)))
  (define (next x)(+ a dx))
   (*dx (sum a b f next)))

(define (id x) x)

(define (sum3-new x) (sum x (expr 10 1000) id exp))

(define (prod a b term next)
  (if (> a b) 1
      (* (term a) (prod (next a) b term next))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))