#lang racket

(define (reverse n)
  (define (helper acc n)
    (if (= n 0) acc
      (helper (+ (* acc 10) (remainder n 10)) (quotient n 10))))
  (helper 0 n))

(define (palindrome? n)
  (let ((rev-n (reverse n)))
    (equal? n rev-n)))

(define (count-palindromes a b)
  (cond ((> a b) 0)
        ((palindrome? a) (+ (count-palindromes (+ a 1) b) 1))
        ((count-palindromes (+ a 1) b))))

(define (prime? n)
  (define (helper i n)
    (cond ((= i n) #t)
          ((and (= (remainder n i) 0) (not (= i 1))) #f)
          (else (helper (+ i 1) n))))
  (helper 1 n))

(define (sum-primes n k)
  (define (helper sum k n)
    (cond ((= n 0) sum)
          ((prime? k) (helper (+ sum k) (+ k 1) (- n 1)))
          (else (helper sum (+ k 1) n))))
  (helper 0 k n))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (take-while p? l)
  (cond ((null? l) '())
        ((p? (car l)) (cons (car l) (take-while p? (cdr l))))
        (else '())))

;(define (prime-factors n)

(define (increasing? l)
  (define (helper curr l)
    (cond ((null? l) #t)
          ((> curr (car l)) #f)
          (else (helper (car l) (cdr l)))))
  (helper (car l) (cdr l)))

(define (progression? l)
  (define (helper d curr l)
    (cond ((null? l) #t)
          ((not (equal? (- (car l) curr) d)) #f)
          (else (helper d (car l) (cdr l)))))
  (helper (- (cadr l) (car l)) (cadr l) (cddr l)))

(define (member? x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
        (else (member? x (cdr l)))))

(define (has-dublicates? l)
  (cond ((null? l) #f)
        ((member? (car l) (cdr l)) #t)
        (else (has-dublicates? (cdr l)))))

(define (dedup l)
  (cond ((null? l) '())
        ((member? (car l) (cdr l)) (dedup (cdr l)))
        (else (cons (car l) (dedup (cdr l))))))

(define (foldl op init l)
  (if (null? l) init
      (foldl op (op init (car l)) (cdr l))))

(define (reversel l)
  (foldl (lambda (x xs) (cons xs x)) '() l))

(define (union xs ys)
  (define (helper xs acc)
    (cond ((null? xs) acc)
          ((member? (car xs) acc) (helper (cdr xs) acc))
          (else (helper (cdr xs) (cons (car xs) acc)))))
  (reversel (helper ys (helper xs '()))))

(define (intersection xs ys)
  (define (helper xs acc)
    (cond ((null? xs) acc)
          ((member? (car xs) ys) (helper (cdr xs) (cons (car xs) acc)))
          (else (helper (cdr xs) acc))))
  (reversel (helper xs '())))

(define (product xs ys)
  (define (helper xs cys acc)
    (cond ((null? xs) acc)
          ((null? cys) (helper (cdr xs) ys acc))
          (else (helper xs (cdr cys) (cons (cons (car xs) (car cys)) acc)))))
  (reversel (helper xs ys '())))

(define (del-assoc alist k)
  (filter (lambda (x) (not (= (car x) k))) alist))

(define (add-assoc alist k v)
  (cons (cons k v) (del-assoc alist k)))

(define (alist-values alist)
  (map (lambda (x) (cdr x)) alist))

(define (count-occ l)
  (define (iter xs alist)
    (cond ((null? xs) alist)
          ((assoc (car xs) alist) (iter (cdr xs) (add-assoc alist (car xs)
                                                            (+ 1 (cdr (assoc (car xs) alist))))))
          (else (iter (cdr xs) (add-assoc alist (car xs) 1)))))
  (iter l '()))

(define (most-common l)
  (car (foldr (lambda (x r) (if (> (cdr x) (cdr r)) x r)) '("ne" . 0) (count-occ l))))


(define (scalar-product xs ys)
  (cond ((or (null? xs) (null? ys)) 0)
        ((+ (* (car xs) (car ys)) (scalar-product (cdr xs) (cdr ys))))))

(define (at n l)
  (cond ((null? l) #f)
        ((= n 0) (car l))
        (else (at (- n 1) (cdr l)))))

(define (matrix-main-diag m)
  (define (helper i m)
    (if (null? m) '()
        (cons (at i (car m)) (helper (+ i 1) (cdr m)))))
  (helper 0 m))

(define (matrix-second-diag m)
  (define (helper i m)
    (if (null? m) '()
        (cons (at i (car m)) (helper (- i 1) (cdr m)))))
  (helper (- (length (car m)) 1) m))

(define (diagonal-product matrix)
  (scalar-product (matrix-main-diag matrix) (matrix-second-diag matrix)))

(define (matrix-multiply m t)
  (map (lambda (row)
         (map (lambda (col)
                (scalar-product row col))
              (apply map list t))) m))

(define (max-ordered-sublist lst)
  (define (helper res curr l el)
    (cond ((null? l) res)
          ((< (car l) el) (if (> (length curr) (length res)) (helper (cons el curr) '() (cdr l) (car l)) (helper res '() (cdr l) (car l))))
          (else (helper res (cons el curr) (cdr l) (car l)))))
  (reversel (helper '() '() (cdr lst) (car lst))))

(define (replace l dict)
  (define (helper res l)
    (cond ((null? l) res)
          ((assoc (car l) dict) (helper (cons (cdr (assoc (car l) dict)) res) (cdr l)))
          (else (helper (cons (car l) res) (cdr l)))))
  (reversel (helper '() l)))

(define (distance p1 p2)
  (sqrt (+ (* (- (car p1) (car p2)) (- (car p1) (car p2)))
           (* (- (cdr p1) (cdr p2)) (- (cdr p1) (cdr p2))))))

(define (closest-point xys)
  (lambda (k)
  (define (helper minDist minPoint xys)
    (cond ((null? xys) minPoint)
          (else (let* ((currPoint (car xys))
                       (curr-dist (distance k currPoint)))
                  (if (< curr-dist minDist) (helper curr-dist currPoint (cdr xys))
                                            (helper minDist minPoint (cdr xys)))))))
    (helper +inf.0 '() xys)))

(define (flatten l)
  (cond ((null? l) '())
        ((not (pair? l)) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(define (position moves)
  (define (helper moves pos)
    (cond ((null? moves) pos)
          (else (let ((dx (car (car moves)))
                      (dy (cdr (car moves))))
            (helper (cdr moves) (cons (+ (car pos) dx)
                                      (+ (cdr pos) dy)))))))
  (helper moves '(0 . 0)))

(define (distance1 moves)
  (define (dist move)
    (sqrt (+ (expt (car move) 2) (expt (cdr move) 2))))
  (foldr (lambda (move acc) (+ (dist move) acc)) 0 moves))

;graphs
(define (adjacency-list nodes edges)
  (map (lambda (v)
         (map (lambda (e) (cdr e))
              (filter (lambda (e) (eqv? v (car e))) edges)))
       nodes))

(define (has-edge u v edges)
  (cond ((null? edges) #f)
        ((and (eqv? u (caar edges)) (eqv? v (cdr (car edges)))) #t)
        (else (has-edge u v (cdr edges)))))
  
  
(define (path? edges nodes)
  (let ((curr (car nodes))
        (n (cdr nodes)))
    (cond ((null? n) #t)
          ((has-edge curr (car n) edges) (path? edges n))
          (else #f))))
       
(define (all-simple-paths adj-list start end)
  (define (helper curr path)
    (cond ((eqv? end curr) (list (reversel (cons curr path))))
          ((not (memv curr path))
            (let ((neighbors (cadar (filter (lambda (x) (eqv? (car x) curr)) adj-list))))
              (if (not (null? neighbors)) (apply append (map (lambda (neigh) (helper neigh (cons curr path))) neighbors))
                  '())))
              
          (else '())))
  (helper start '()))

(define adj-list '((1 (2 4)) (2 (3)) (3 (4)) (4 ()))) ;;((1 (2 4))