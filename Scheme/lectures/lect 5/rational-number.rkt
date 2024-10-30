(define (make-rat n d)
  (cons 'rat
  (if (= d 0)
      (cons n 1)
      (if (< d 0) (make-rat (- n) (- d))
      (let* ((g (gcd n d))
             (ng (quotient n g))
             (dg (quotient d g)))
      (cons ng dg))))))

(define get-numer cadr)
(define get-denom cddr)

(define (*rat p q)
  (make-rat
   (* (get-numer p) (get-numer q))
   (* (get-denom p) (get-denom q))))

(define (+rat p q)
  (make-rat
   (+ (* (get-numer p) (get-denom q))
      (* (get-numer q) (get-denom p)))
   (* (get-denom p) (get-denom q))))

(define (<rat p q)
  (< (* (get-numer p) (get-denom q))
     (* (get-numer q) (get-denom p))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
   (op (term a) (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))

(define (fact x)
   (accumulate * 1 1 x (lambda (i) i) 1+))

(define (pow x k)
  (accumulate * 1 1 k (lambda (i) x) 1+))

(define (todouble r)
  (+ .0 (/ (get-numer r) (get-denom r))))

(define (my-exp x n)
  (accumulate
   +rat (make-rat 0 1) 0 n
   (lambda (i) (make-rat (pow x i) (fact i)))  1+))

(define (rat? p)
  (and (pair? p) (eqv? (car p) 'rat)
       (pair? (cdr p))
       (integer? (cadr p)) (positive? (cddr p))
       (= (gcd (cadr p) (cddr p)) 1)))

(define (make-rat n d)
  (let* ((d (if (= 0 d) 1 d))
        (sign (if (> d 0) 1 -1))
        (g (gcd n d))
        (numer (* sign (quotient n g)))
        (denom (* sign (quotient d g))))
  (define (self prom.params)
    (case prop
      ('get-numer numer)
      ('get-denom denom)
      ('print (cons numer denom))
      ('* (let ((r (car params)))
            (make-rat (* (self 'get-numer) (r 'get-numer))
                      (* (self 'get-denom) (r 'get-denom)))))                                     
      (else 'unknown-prop)))
       self))

  