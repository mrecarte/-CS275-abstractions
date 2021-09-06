#lang racket
; Definition of tree datatype
(provide tree make-tree empty-tree
         tree? empty-tree? leaf?
         tree-value tree-children)
(provide empty-tree T1 T2 T3 T4 T5 T6 T7 T8)

; Constructors

; An empty tree is represented by null, just as an empty list is.
(define empty-tree null)

; Create a tree with the given value and children.
(define (tree value children)
  (list 'tree value children))

; Convenience constructor
; (make-tree v c1 c2 ... cn) is equivalent to
; (tree v (list c1 c2 ... cn))
(define (make-tree value . children)
  (tree value children))
  
; Recognizers

; Returns #t if t is an empty tree.
(define (empty-tree? t)
  (null? t))

; Returns #t if t is a tree (either empty or not).
(define (tree? t)
  (cond [(empty-tree? t) #t]
        [(list? t) (eq? (first t) 'tree)]
        [else #f]))


; Accessors

; Returns the tree's value. t must be a nonempty tree.
(define (tree-value t)
  (cond [(empty-tree? t) (error 'tree-value "argument is an empty tree")]
        [(tree? t) (second t)]
        [else (error 'tree-value "~s is not a tree" t)]))

; Returns the tree's children. t must be a nonempty tree.
(define (tree-children t)
  (cond [(empty-tree? t) (error 'tree-children "argument is an empty tree")]
        [(tree? t) (third t)]
        [else (error 'tree-children "~s is not a tree" t)]))

; Utility procedure

; Returns #t if the tree is a leaf. That is, if it has no children.
(define (leaf? t)
  (cond [(empty-tree? t) #f]
        [(not (tree? t)) (error 'leaf? "~s is not a tree" t)]
        [else (empty? (tree-children t))]))


; Example trees 

(define T1 (make-tree 50)) 
(define T2 (make-tree 22)) 
(define T3 (make-tree 10)) 
(define T4 (make-tree 5)) 
(define T5 (make-tree 17)) 
(define T6 (make-tree 73 T1 T2 T3)) 
(define T7 (make-tree 100 T4 T5)) 
(define T8 (make-tree 16 T6 T7)) 

