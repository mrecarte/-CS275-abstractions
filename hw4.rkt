#lang racket
; Marilyn Recarte

; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))
(require "TreeDatatype.rkt")

;1

(define index (lambda (x lst)
                (let
                    ([index (foldr(lambda (el accu) (if (eq? el x) 0 (+ 1 accu))) -1 lst)])
                  (if (< (+ index 1) (length lst)) index -1))))

;2

(define replace
  (lambda (a b lst)
    (foldr (lambda(x y)
             (if (equal? x a) (cons b y) (cons x y)))
           null lst)))

;3

(define bags '((duffle 8)
               (garment-bag 2)
               (briefcase 5)
               (valise 7)
               (steamer-trunk 65)))

(define sublist
  (lambda (lst)
    (cond
      [(null? lst) null]
      [else (cons (cadr (first lst)) (sublist (rest lst)))])))

(define weigh
  (lambda (bags)
    (foldr + 0 (sublist bags))))

;4

(define max
  (letrec([max-a (lambda (lst acc)
                   (cond
                     [(null? lst) acc]
                     [(> (cadr (first lst)) (cadr acc)) (max-a (rest lst) (first lst))]
                     [else (max-a (rest lst) acc)]))])
    (lambda (lst) (max-a lst '(null 0)))))
    
(define heaviest
  (lambda (bags)
    (foldr (lambda (x y)
             (if (= (cadr x) (cadr (max bags))) (first x) y))
           null bags)))

;5

(define child-sum (lambda (t)
                    (cond
                      [(empty-tree? t) 0]
                      [(leaf? t) 0]
                      [else (apply + (map tree-value (tree-children t)))])))

;6

(define all-sum (lambda (t)
                  (cond
                    [(empty-tree? t) 0]
                    [else (apply + (cons (tree-value t) (map all-sum (tree-children t))))])))

;7

(define visit-tree
  (lambda (f t)
    (cond
      [(null? t) null]
      [(number? t) (f t)]
      [(atom? t) t]
      [else (map(lambda(v)(visit-tree f v))t)])))

;8

(define atom? (lambda (x)
                (not (list? x))))

(define sizeof 
  (lambda (t)
    (cond
      [(null? t) 0]
      [(number? t) 1]
      [(atom? t) 0]
      [else (apply + (map sizeof t))])))

;9

(define (maximum lst)
  (if (null? (cdr lst)) 
      (car lst) 
      (if (< (car lst) (maximum (cdr lst)))  
          (maximum (cdr lst)) 
          (car lst))))

(define height 
  (lambda (t)
    (cond
      [(empty-tree? t) -1]
      [(leaf? t) 0]
      [else (+ 1 (maximum (map height (tree-children t))))])))

;10

(define preorder
  (lambda (t)
    (cond
      [(leaf? t) (list (tree-value t))]
      [else (cons (tree-value t) (apply append (map preorder (tree-children t))))])))

;11

(define postorder
  (lambda (t)
    (cond
      [(leaf? t) (list (tree-value t))]
      [else (append (apply append (map postorder (tree-children t))) (list (tree-value t)))])))