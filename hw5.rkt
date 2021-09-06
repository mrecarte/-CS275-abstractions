#lang racket
;Marilyn Recarte
; Export all of our top-level definitions so that tests.rkt
; can import them. See tests.rkt.
(provide (all-defined-out))

;1

(define backtrack (lambda (subset goal params sofar)
                    (cond
                      [(= subset goal) sofar]
                      [(> subset goal) null]
                      [(null? params) #f]
                      [else (let
                                ([res (backtrack (+ subset (first params)) goal (rest params) (cons (first params) sofar))])
                              (if (null? res) (backtrack subset goal (rest params) sofar) res))])))

(define subset-sum (lambda (goal params)
                     (backtrack 0 goal params null)))
;2

(define feasable? (lambda (curr lst)
              (cond
                [(null? lst) #t]
                [(list-prefix? curr lst) #f]
                [else (feasable? (append curr (list (first lst))) (rest lst))])))

(define backtrack2 (lambda (curr numsleft sofar)
                             (cond
                               [(= 0 numsleft) sofar]
                               [(> curr 3) #f]
                               [(< numsleft 0) null]
                               [(feasable? (list curr) sofar)
                                (let
                                    ([res (backtrack2 1 (- numsleft 1) (cons curr sofar))])
                                  (if res
                                   res (backtrack2 (+ curr 1) numsleft sofar)))]
                               [else
                                (backtrack2 (+ curr 1) numsleft sofar)])))

(define no-repeat (lambda (n)
                   (backtrack2 1 n null)))
