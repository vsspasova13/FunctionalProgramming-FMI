(define (fact n)(if(= n 0) 1
                   (* (fact(- n 1)) n)))

(define (add a b) (+ a b))

(define (count-digits n) (if (< n 10) 1
                             (+ 1 (count-digits(quotient n 10)))))

(define (sum-digits n) (if (< n 10) n
                           (+ (modulo n 10) (sum-digits(quotient n 10)))))

(define (list-sum list) (if (null? list) 0
                            (+ (car list) (list-sum(cdr list)))))

