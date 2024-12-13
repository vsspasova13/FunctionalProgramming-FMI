#lang racket

(define (accumulate op nv a b term next)
  (if (> a b) nv
          (op (term a) (accumulate op nv (next a) b term next))))

(define 1+ (lambda (x) (+ x 1)))

(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))
       (all? list? m)
       (all? (lambda (row) (= (length row)
                       (length (car m)))) m)))
  
(define get-rows length)
(define (get-columns m) (length (car m)))

(define get-first-row car)
(define (get-first-column m)(map car m))

(define del-first-row cdr)
(define (del-first-col m) (map cdr m))

(define get-row list-ref)
(define (get-column m i)
  (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (accumulate cons '() 0 (- (get-columns m) 1)
              (lambda (i) (get-column m i)) 1+))

(define (+vectors v1 v2) (map + v1 v2))
(define (+matrices m1 m2) (map +vectors m1 m2))

(define (*vectors v1 v2) (apply + (map * v1 v2)))
(define (*matrices m1 m2)
  (let ((m2t (transpose m2)))
       (map (lambda (row)
              (map (lambda (column) (*vectors row column))
                   m2t))
            m1)))


(define (tree? t)
  (or (null? t)
      (and (list t) (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (depth-tree t)
  (if (empty-tree? t) 0
      (1+ max((depth-tree (left-tree t))
              (depth-tree (right-tree t))))))

(define (memv-tree x t)
  (and (not (empty-tree? t))
       (or (and (eqv? x (root-tree t)) t)
           (memv-tree x (left-tree t))
           (memv-tree x (right-tree t)))))

(define (cons#f h t) (and t (cons h t)))

(define (path-tree x t)
  (and (not (empty-tree? t))
       (or (and (eqv? x (root-tree t)) (list x))
           (cons#f (root-tree t)
                   (or (path-tree x (left-tree t))
                       (path-tree x (right-tree t)))))))

(define tree '(1 (2 (4) (5)) (3 () (6))))

(define (make-alist f keys)
(map (lambda (x) (cons x (f x))) keys))

(define (keys alist) (map car alist))
(define (values alist) (map cdr alist))

(define (search p l)
(and (not (null? l))
(or (p (car l)) (search p (cdr l)))))

(define vertices keys)

(define (children v g)
  (cdr (assv v g)))

(define (edge? u v g)
  (memv v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (search-child v f g)
  (search f (children v g)))

(define (childless g)
  (filter (lambda (x) (null? (children x g))) (vertices g)))

(define (parents v g)
  (filter (lambda (x) (edge? v x)) (vertices g)))

(define (symetric? g)
  (all? (lambda (u)
          (all? (lambda (v) (edge? v u g))
                (children u g)))
        (vertices g)))

(define (dfs node graph visited)
  (if (member node visited) (reverse visited)
      (foldl (lambda (neighbor acc)
               (dfs neighbor graph acc))
             (cons node visited)
             (children node graph))))

(define (bfs start graph)
  (define (helper queue visited)
    (if (null? queue) (reverse visited)
        (let ((current (car queue)))
          (if (member current visited) (helper (cdr queue) visited)
              (helper (append (children current graph) (cdr queue))
                      (cons current visited))))))
  (helper (list start) '()))


(define graph '((1 . (2 3))
                (2 . (4))
                (3 . (4 5))
                (4 . ())
                (5 . (6))
                (6 . ())))


(define (is-connected? g)
  (let* ((start (car (car g)))
         (visited (dfs start g '())))
    (= (length visited) (length g))))


(define (all-paths u v g)
  (define (helper current path visited)
    (if (= current v)  
        (list (reverse path))  
        (if (member current visited) 
            '()
            (let ((neighbors (children current g)))  
              (foldl append '()  
                    (map (lambda (neighbor) (helper neighbor (cons neighbor path) (cons current visited)))
                         neighbors))))))
  (helper u (list u) '())) 











