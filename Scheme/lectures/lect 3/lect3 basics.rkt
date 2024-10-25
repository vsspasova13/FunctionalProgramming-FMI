(define (pow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (pow x (-n))))
        (else (* x (pow x (- n 1))))))


(define (qpow x n)
 (define (sqr x) (* x x))
  (define (even? x) (= (remainder x 2) 0))
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (qpow x (- n))))
        ((even? n) (sqr (qpow x (quotient n 2))))
        (else (* x (qpow x (- n 1))))))


(define (fibb2 n)
  (define (iter i fi fi-1)
    (if (= i n) fi
        (iter (+ i 1) (+ fi fi-1) fi)))
  (if (= n 0) 0
      (iter 1 1 0)))