#lang racket
; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

;1
(define (firsts lsts)
  (map first lsts))

(define (rests lsts)
  (map rest lsts))

;2
(define (vec-+ vec1 vec2)
  (map + vec1 vec2))

;3
(define (dot-product ls1 ls2)
  (cond ((empty? ls1) 0)
        ((empty? ls2) 0)
        (else
         (apply + (map * ls1 ls2)))))

;4
(define (mat-vec-* mat vec)
  (map (lambda(x)(dot-product x vec)) mat))
;5

;5
(define (transpose mat)
  (apply map list mat))

;6

(define (mat-mat-* m1 m2)
  (for/list ([r m1])
    (for/list ([c (apply map list m2)])
      (apply + (map * r c)))))

;7

(define (flatten lst)
  (cond
    [(empty? lst) empty]
    [(list? lst)
     (append (flatten (first lst))
             (flatten (rest lst)))]
    [else (list lst)]))

;8
(define (sum lst)
  (apply + (flatten lst)))

;9

(define (map-to f lst)
  (cond
    [(empty? lst) empty]
    [(list? (first lst))
     (cons (map-to f (first lst)) (map-to f (rest lst)))]
    [else
     (cons (f (first lst)) (map-to f (rest lst)))]))

;10

(define (element-of? a lst)
  (define (helper a lst)
    (if (null? lst) #f (or (equal? a (first lst)) (helper a (rest lst)))))
  (helper a (flatten lst)))