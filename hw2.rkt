#lang racket
; Marilyn Recarte and Nashleen Salazar Rodriguez
; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt
(provide (all-defined-out))

;1
(define (merge lst1 lst2)
  (cond ((null? lst1) lst2) ;checks if lst1 is empty, then returns lst2
        ((null? lst2) lst1) ;checks if lst2 is empty, then returns lst1
        ((>= (car lst1)(car lst2)) ;compares the first values of lst1 and lst 2
             (cons (car lst2)(merge lst1 (cdr lst2)))) ;if lst 2 is greater then lst2, then recurse using lst1 and the rest of lst2
        (else (cons(car lst1)(merge (cdr lst1) lst2))))) ;if lst 1 is greater than lst1, then recurse using lst2 and the rest of lst1
;2

(define (insert n lst)
  (cond ((null? lst)(cons n lst))
     ((<= n (first lst))(cons n lst))
         (else
          (cons (first lst)(insert n (rest lst))))))

(define (sort lst)
  (cond ((null? lst) lst)
        (else (insert (first lst)(sort(rest lst))))))

;3

(define (contains-sublist? sublist biglist)
  (cond [(> (length sublist) (length biglist)) #f]
        [(empty? sublist) #t]
        [else (and (if (equal? (first sublist) (first biglist)) #t (contains-sublist? sublist (rest biglist))) (contains-sublist? (rest sublist) (rest biglist)))]))
;4

(define (remove-sublist sublist biglist)
  (cond [(or (empty? sublist) (> (length sublist) (length biglist))) biglist]
        [(equal? (first sublist) (first biglist)) (remove-sublist (rest sublist) (rest biglist))]
        [else (cons (first biglist) (remove-sublist sublist (rest biglist)))]))


;5
(define phone-book
  '((barbara 775-1234)
    (luke 774-2839)
    (nick 775-0912)
    (valerie 775-9043)))

(define (phone-number person phone-book)
  (cond [(empty? phone-book) 'disconnected]
        [(equal? (first (first phone-book)) person) (second (first phone-book))]
        [else (phone-number person (rest phone-book))]))

;6

(define (person phone-number phone-book)
  (cond [(empty? phone-book) 'disconnected]
        [(equal? (second (first phone-book)) phone-number) (first (first phone-book))]
        [else (person phone-number (rest phone-book))]))

;7

(define (deepen lst)
  (map (lambda (item) (cons item empty)) lst))

;8

(define (eval-bin lst)
  (define (helper list total)
   (cond [(empty? list) total]
         [else (helper (rest list) (+ (* total 2) (first list)))]))
  (helper lst 0))

;9

(define (sub old new lst)
  (cond [(null? lst) '()]
        [(equal? old (first lst))
         (cons new (sub old new (rest lst)))]
        [else
         (cons (first lst) (sub old new (rest lst)))]))

;10

(define (subs old-lst new-lst lst)
  (cond [(empty? lst) empty]
        [(or (empty? old-lst) (empty? new-lst)) lst]
        [else (subs (rest old-lst) (rest new-lst) (sub (first old-lst) (first new-lst) lst))]))
