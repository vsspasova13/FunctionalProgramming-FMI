(define (square x) (* x x))

(define (three-digit-number? x)(if (and (> x 99) (< x 1000)) #t #f))

(define (fibb x) (cond ((= x 0) 1)
                       ((= x 1) 1)
                       (else (+ (fibb (- x 1)) (fibb (- x 2))))))

(define (first-digit x) (cond ((= x 0) 0)
                             ((< x 10) x)
                          (else   (last-digit (quotient x 10)))))