(define (square x) (* x x))

(define (three-digit-number? x)(if (and (> x 99) (< x 1000)) #t #f))

(define (fibb x) (cond ((= x 0) 1)
                       ((= x 1) 1)
                       (else (+ (fibb (- x 1)) (fibb (- x 2))))))

(define (first-digit x) (cond ((= x 0) 0)
                             ((< x 10) x)
                          (else   (last-digit (quotient x 10)))))

(define (sum-cubes x y) (+ (* x x x) (* y y y)))

(define (my-max-guard a b) (cond ((> a b)a)
                              (else b)))

(define (my-max-built-in a b) (max a b))

(define (my-max-if a b)(if (> a b) a
                           b))

(define (not-equal-one-line? a b) (not(= a b)))

(define (between? a b c) (or (and (> a c) (< b c)) (and (< a c) (> b c))))

(define (leap-year-one-line? y) (or (= (remainder y 400) 0) (and (= (remainder y 4) 0) (not (= (remainder y 100) 0)))))

(define (leap-year-guards? y) (cond ((= (remainder y 400) 0))
                                    (else (and (= (remainder y 4) 0) (not (= (remainder y 100) 0))))))
(define (my-gcd a b) (cond ((= b 0)a)
                          (else (my-gcd b (remainder a b)))))
  
(define (help-gosho a) (cond ((and (<= (- 15 a) 15) (>= (- 15 a) 10)) "Pizza")
                             ((and (<= (- 15 a) 9.99) (>= (- 15 a) 5)) "Doner")
                             ((and (<= (- 15 a) 4.99) (>= (- 15 a) 3)) "University cafeteria")
                             (else "Nothing")))

(define (help-students x y) (cond ((and (= x 0) (= y 0)) "Center")
                                  ((= x 0) "Ordinate")
                                  ((= y 0) "Abscissa")
                                  ((and (> x 0) (> y 0)) "1 Quadrant")
                                  ((and (> x 0) (< y 0)) "2 Quadrant")
                                  ((and (< x 0) (> y 0)) "4 Quadrant")
                                  ((and (< x 0) (< y 0)) "3 Quadrant")))

(define (mirror? x) (if (and (= (quotient x 1000) (remainder x 10)) (= (remainder (quotient x 100) 10) (remainder (quotient x 10) 10))) #t #f))

