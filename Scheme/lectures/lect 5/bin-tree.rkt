#lang racket

(define empty-tree '())
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (make-leaf x) (make-tree x empty-tree empty-tree))

(define make-tree list)

(define t (make-tree 1 (make-leaf 2)
                       (make-tree 3 (make-leaf 4)
                                    (make-leaf 5))))
(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define (depth-tree t)
  (if (empty-tree? t) 0
      (+ 1
        (max (depth-tree (left-tree t))
             (depth-tree (right-tree t))))))

(define (memv-tree x t)
  (and (not (empty-tree? t))
       (or (and (equal? x (root-tree t)) t)
           (memv-tree x (left-tree t))
           (memv-tree x (right-tree t)))))

(define (cons#f h t)
  (and t (cons h t)))

(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (root-tree t) ) (list x))
        (else (cons#f (root-tree t)
                      (or (path-tree x (left-tree t))
                           (path-tree x (right-tree t)))))))
              
(define (path-tree2 x t)
  (and (not (empty-tree? t))
       (or (and (eqv? x (root-tree t)) (list x))
           ((cons#f (root-tree t)
                      (or (path-tree x (left-tree t))
                           (path-tree x (right-tree t))))))))


