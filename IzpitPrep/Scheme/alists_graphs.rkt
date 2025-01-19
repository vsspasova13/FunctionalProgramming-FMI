#lang racket

(define (all? p l)
  (cond ((null? l) #t)
        ((not (p (car l))) #f)
        (else (all? p (cdr l)))))

(define (cons#f x l) (and l (cons x l)))

;alist

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

(define al (make-alist (lambda (x) (* x x)) '(1 2 3)))

(define (search p? l)
  (let ((result (filter p? l)))
    (if (not (null? result)) (car result) #f)))

(define (search2 p l)
  (cond ((null? l) #f)
        ((p (car l)) (p (car l)))
        (else (search p (cdr l)))))

(define (search3 p l)
  (and (not (null? l)) (or (p (car l)) (search3 p (cdr l)))))


;graphs
(define g '((1 2 3) (2 3) (3 4 5) (4) (5 2 4 6) (6 2)))

(define (vertices g) (keys g))
(define (children v g) (cdr (assq v g)))

(define (edge? u v g)
  (memv v (children u g)))

(define (map-children f u g) (map f (children u g)))

(define (search-children p u g) (search3 p (children u g)))

(define (childless g)
  (filter (lambda (x) (null? (children x g))) (vertices g)))


(define (parents v g)
  (filter (lambda (u) (member v (children u g))) (vertices g)))

(define (symmetric? g)
  (all? (lambda (u)
          (all? (lambda (v)
                (edge? v u g))
          (children u g)))
        (vertices g)))

(define (dfs? u v g)
  (or (eqv? u v) (search-children (lambda (w) (dfs? w v g)) u g)))

;only for acyclic graph
(define (dfs u v g)
  (or (and (eqv? u v) (list u))
      (search-children (lambda (w) (cons#f u (dfs w v g))) u g)))

;for cyclic
(define (dfs-path u v g)
  (define (dfs-search path)
    (let ((curr (car path)))
      (if (eqv? curr v) (reverse path)
          (search-children (lambda (w) (and (not (memv w path)) (dfs-search (cons w path)))) curr g)

    )))
  (dfs-search (list u)))

(define (bfs u v g)
  (define (extend path)
    (map-children (lambda (w) (cons w path)) (car path) g))

  (define (remains-acyclic? p)
    (not (memv (car p) (cdr p))))

  (define (extend-acyclic path)
    (filter remains-acyclic? (extend path)))

  (define (extend-level l)
    (apply append (map extend-acyclic l)))
  
  (define (target-path path)
    (and (eqv? (car path) v) path))
  
  (define (bfs-level l)
    (if (null? l) #f
        (or (search target-path l)
            (bfs-level (extend-level l)))))

  (bfs-level (list (list u))))
    
  


