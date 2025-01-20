#lang racket
(define (all? p l)
  (cond ((null? l) #t)
        ((not (p (car l))) #f)
        (else (all? p (cdr l)))))

;trees

(define make-tree list)
(define empty-tree '())
(define (make-leaf x) (make-tree x empty-tree empty-tree))
(define root car)
(define left cadr)
(define right caddr)
(define is-empty? null?)

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (right t))
           (tree? (left t)))))

(define (leaf? t)
  (or (null? t) 
      (and (list? t)
           (= (length t) 3)
           (null? (right t))
           (null? (left t)))))

(define t (make-tree 1 (make-leaf 2) (make-tree 3 (make-leaf 4)
                                                  (make-leaf 5))))

(define (pre-order t)
  (if (is-empty? t) '()
      (append  (list (root t)) (pre-order (left t)) (pre-order (right t)))))

(define (in-order t)
  (if (is-empty? t) '()
      (append (in-order (left t)) (list (root t)) (in-order (right t)))))

(define (post-order t)
  (if (is-empty? t) '()
      (append (post-order (left t)) (post-order (right t)) (list (root t)))))

(define (map-tree f t)
  (if (is-empty? t) '()
      (make-tree (f (root t)) (map-tree f (left t)) (map-tree f (right t)))))

(define (max1 a b)
  (if (> a b) a b))

(define (height t)
  (if (is-empty? t) 0
      (+ (max1 (height (left t)) (height (right t))) 1)))

(define (level n t)
  (cond ((is-empty? t) '())
        ((= n 0) (list (root t)))
        (else (append (level (- n 1) (left t)) (level (- n 1) (right t))))))

(define (count-leaves t)
  (cond ((is-empty? t) 0)
        ((leaf? t) 1)
        (else (+ (count-leaves (left t)) (count-leaves (right t))))))


(define (remove-leaves t)
  (cond ((is-empty? t) '())
        ((leaf? t) '())
        (else (make-tree (root t) (remove-leaves (left t)) (remove-leaves (right t))))))

(define (invert t)
  (cond ((is-empty? t) '())
        (else (make-tree (root t) (invert (right t)) (invert (left t))))))

(define (bst? t)
  (cond ((is-empty? t) #t)
        ((leaf? t) #t)
        ((not (and (< (root (left t)) (root t)) (> (root (right t)) (root t)))) #f)
        (else (and (bst? (left t)) (bst? (right t))))))

(define bst (make-tree 4 (make-tree 2
                               (make-leaf 1)
                               (make-leaf 3))
                         (make-tree 6
                               (make-leaf 5)
                               (make-leaf 7))))

(define (insert-bst x t)
  (if (is-empty? t) (make-leaf x)
        (if (< x (root t)) (make-tree (root t) (insert-bst x (left t)) (right t))
            (make-tree (root t) (left t) (insert-bst x (right t))))))

(define (balanced? t)
  (if (is-empty? t) #t
      (let ((maxH (if(> (height (left t)) (height (right t))) (height (left t)) (height (right t))))
            (minH (if(< (height (left t)) (height (right t))) (height (left t)) (height (right t)))))
      (and (< (- maxH minH) 2) (balanced? (left t)) (balanced? (right t))))))

(define balanced-tree
  (make-tree 4
             (make-tree 2
                        (make-leaf 1)
                        (make-leaf 3))
             (make-tree 6
                        (make-leaf 5)
                        (make-leaf 7))))

(define unbalanced-tree
  (make-tree 1
             empty-tree
             (make-tree 2
                        empty-tree
                        (make-tree 3
                                   empty-tree
                                   (make-tree 4
                                              empty-tree
                                              (make-leaf 5))))))

;alists

(define (keys alist) (map car alist))

(define (values alist) (map cdr alist))

(define (alist? al) (and (list? al) (all? pair? al)))

(define (make-alist f keys)
  (map (lambda (x) (cons x (f x))) keys))

(define (assoc key al)
  (cond ((null? al) #f)
        ((equal? (caar al) key) (car al))
        (else (assoc key (cdr al)))))

(define (del-assoc key al)
  (cond ((null? al) al)
        ((equal? (caar al) key) (cdr al))
        (else (cons (car al) (del-assoc key (cdr al))))))

(define (add-assoc key value al)
  (if (assoc key al) (map (lambda (kv) (if (equal? (car kv) key) (cons key value) kv) ) al)
      (cons (cons key value) al)))

(define al (make-alist (lambda (x) (* x x)) '(1 2 3 3 1)))

(define (enumerate l)
  (define (helper i al l)
    (if (null? l) (reverse al)
        (helper (+ i 1) (cons (cons i (car l)) al ) (cdr l))))
  (helper 0 '() l))

(define (histogram l)
  (define (helper curr res)
    (if (assq curr res) (map (lambda (kv) (if (eqv? (car kv) curr) (cons curr (+ 1 (cdr kv))) kv)) res)
        (cons (cons curr 1) res)))
  (reverse (foldl (lambda (x acc) (helper x acc)) '() l)))

(define (dedup al)
  (cond ((null? al) '())
        ((assoc (caar al) (cdr al)) (cons (cons (caar al) (cdar al)) (dedup (del-assoc (caar al) (cdr al)))))
        (else (cons (cons (caar al) (cdar al)) (dedup (cdr al))))))

(define (merge op al1 al2)
  (cond ((null? al1) al2)
        ((null? al2) al1)
        ((assoc (caar al1) al2) (cons (cons (caar al1) (op (cdar al1) (cdr (assoc (caar al1) al2)))) (merge op (cdr al1) (del-assoc (caar al1) al2))))
        (else (cons (car al1) (merge op (cdr al1) al2)))))

(define (group-by f l)
  (define (insert-key key val result)
    (cond ((null? result) (list (cons key (list val))))
          ((= key (car (car result))) (cons (cons key (append (cdr (car result)) (list val))) (cdr result)))
          (else (cons (car result) (insert-key key val (cdr result))))))
  
  (define (helper l res)
    (cond ((null? l) res)
          (else (let ((key (f (car l))))
                  (helper (cdr l) (insert-key key (car l) res))))))
  (helper l '()))

(define (run-length-encode l)
  (define (helper res l curr count)
    (cond ((null? l) res)
          ((= (length l) 1) (if (not (eqv? (car l) curr)) (cons (cons (car l) 1)(cons (cons curr count) res)) (cons (cons curr (+ 1 count)) res)))
          ((eqv? (car l) curr) (helper res (cdr l) curr (+ count 1)))
          (else (helper (cons (cons curr count) res) (cdr l) (car l) 1))))
  (reverse (helper '() (cdr l) (car l) 1)))


;streams
(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

(define (iterate f x)
  (cons-stream x (iterate f (f x))))

(define nats (iterate (lambda (x) (+ x 1)) 0))

(define (stream-take n s)
  (if (= n 0) the-empty-stream
      (cons (head s) (stream-take (- n 1) (tail s)))))

(define (map-stream f s)
  (cons-stream (f (head s)) (map-stream f (tail s))))

(define (filter-stream p? s)
  (if (p? (head s)) (cons-stream (head s) (filter-stream p? (tail s)))
      (filter-stream p? (tail s))))

(define (isPrime? n)
   (define (helper i)
        (cond ((= i n) #t)
              ((= (remainder n i) 0) #f)
              (else (helper (+ i 1)))))
  (if (< n 2) #f
      (helper 2)))

(define primes1 (filter-stream isPrime? nats))

(define (from n) (cons-stream n (from (+ n 1))))

(define (nondividable n) (lambda (x) (> (remainder x n) 0)))

(define (sieve s)
  (cons-stream (head s)
               (sieve (filter-stream (nondividable (head s)) (tail s)))))

(define primes2 (sieve (from 2)))