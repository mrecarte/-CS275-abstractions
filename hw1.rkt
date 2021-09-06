#lang racket
; Your name(s) here
;Marilyn Recarte and Nashleen Salazar Rodriguez
; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

;1

(define atom?
  (lambda (x)
    (and
     (not (null? x))
     (not (pair? x)))))

;2

(define (list-of-atom? lst)
  (cond [(empty? lst) #t]
        [(atom? (first lst)) (list-of-atom? (rest lst))]
        (else #f)))

;3
 
(define (not-list-of-atom? lst)
  (cond ((null? lst) #f)
        ((not (atom? (first lst))) #t)
        (else
         #f)))

;4

(define (list-of-int? lst)
  (cond ((empty? lst) #t)
        ((integer? (first lst)) (list-of-int? (rest lst)))
        [else #f]))
;5

(define (list-of-same? pred lst)
  (cond [(empty? lst) #t]
        [(pred (first lst)) (list-of-same? pred (rest lst))]
        [else #f]))

;6

(define make-list-of-same?
  (lambda (pred)
    (lambda(lst)
      (list-of-same? pred lst))))

;7

(define (all-members lst1 lst2)
  (cond [(empty? lst1) #t]
        [(empty? lst2) #f]
        [(not (equal? (first lst1) (first lst2))) (all-members lst1 (rest lst2))]
        [(equal? (first lst1) (first lst2)) (all-members (rest lst1) lst2)]))
      

;8

(define (remove-second x lst)
  (cond
    ((null? lst) '())
    ((null? (cdr lst)) lst)
    (else (if (equal? x (car lst))
              (cons (car lst) (remove x (cdr lst)))
              (cons (car lst) (remove-second x (cdr lst)))))))

;9
;fix
(define (remove-pair x lst)
  (cond
    ((equal? lst '()) lst)
    ((not (equal? x (first lst))) (cons (first lst) (remove-pair x (rest lst))))
    ((and (>(length lst) 1)
          (equal? x (first lst))
          (equal? x (second lst)))
     (remove-pair x(remove x (rest lst))))
    (else ( cons (first lst) (remove-pair x (rest lst))))))

 
;10

(define (duplicate n exp)
  (cond
    [(zero? n) empty]
    [else (cons exp (duplicate (sub1 n) exp))]))

;11

(define (maximum lst)
  (if (null? (cdr lst)) 
      (car lst) 
      (if (< (car lst) (maximum (cdr lst)))  
          (maximum (cdr lst)) 
          (car lst))))

;12

(define (index-of lst x)
  (cond ((equal? lst '()) #f)
        ((equal? x (first lst)) 0)
        (else
         ((lambda (result)
            (cond
              ((equal? result #f) #f)
              (else
               (+ 1 result)))) (index-of (rest lst) x)))))
