#lang racket

(provide env
         env?
         empty-env
         empty-env?
         extended-env?
         env-syms
         env-vals
         env-previous
         env-lookup)

; The empty environment is null.
(define empty-env null)

; Environment constructor.
(define (env syms vals previous-env)
  (cond [(not (list? syms)) (error 'env "syms is not a list")]
        [(not (list? vals)) (error 'env "vals is not a list")]
        [(not (env? previous-env)) (error 'env "previous-env is not an env")]
        [else (list 'env syms (map box vals) previous-env)]))

; Environment recognizers.
(define (env? e)
  (or (empty-env? e) (extended-env? e)))

(define (empty-env? e)
  (null? e))

(define (extended-env? e)
  (and (list? e)
       (not (null? e))
       (eq? (first e) 'env)))

; Environment accessors.
(define (env-syms e)
  (cond [(empty-env? e) empty]
        [(extended-env? e) (second e)]
        [else (error 'env-syms "e is not an env")]))

(define (env-vals e)
  (cond [(empty-env? e) empty]
        [(extended-env? e) (third e)]
        [else (error 'env-vals "e is not an env")]))

(define (env-previous e)
  (cond [(empty-env? e) (error 'env-previous "e has no previous env")]
        [(extended-env? e) (fourth e)]
        [else (error 'env-previous "e is not an env")]))

(define env-a
  (env '(x y) '(1 2) empty-env))
(define env-b
  (env '(x z) '(5 7) env-a))

(define env-lookup
  (lambda (env symbol)
    (cond [(empty-env? env) (error 'env-lookup "No binding for ~s" symbol)]
          [(index-of (env-syms env) symbol)
          (list-ref (env-vals env)(index-of(env-syms env)symbol))]
          [else (env-lookup (env-previous env) symbol)])))
  