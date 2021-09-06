#lang racket
(require "env.rkt")
(require "parse.rkt")

(provide (all-defined-out))

;eval-exp
(define eval-exp (lambda (tree e)
                   (cond
                     [(lit-exp? tree) (lit-exp-num tree)]
                     [(var-exp? tree)
                      (unbox (env-lookup e (var-exp-symbol tree)))]
                     [(app-exp? tree)
                      (let ([proc (eval-exp (app-exp-proc tree) e)]
                            [args (map (lambda (arg) (eval-exp arg e))(app-exp-args tree))])
                        (apply-proc proc args))]
                     [(ite-exp? tree)
                      (let ([cond (eval-exp (ite-exp-cond tree) e)])
                        (if (or (equal? 0 cond) (equal? 'False cond))
                            (eval-exp (ite-exp-else tree) e)
                            (eval-exp (ite-exp-then tree) e)))]
                     [(let-exp? tree)
                      (let ([new-env (env (let-exp-symbols tree) (map (lambda (exp)
                                                                     (eval-exp exp e))
                                                                   (let-exp-vals tree)) e)])
                        (eval-exp (let-exp-body tree) new-env))]
                     [(lambda-exp? tree)
                      (closure (lambda-exp-params tree)
                               (lambda-exp-body tree)
                               e)]
                     [(set-exp? tree)
                      (set-box! (env-lookup e (set-exp-symbol tree))
                                (eval-exp (set-exp-value tree) e))]
                     [(begin-exp? tree)
                      (foldl (λ (exp acc) (eval-exp exp e))
                             (void)
                             (get-begin-exps tree))]
                     [else (error 'eval-exp "Invalid tree: ~s" tree)])))

; closure data type
(define (closure param-list body env)
  (list 'closure param-list body env))

(define (closure? obj)
  (if (list? obj)
      (if (eq? 'closure (first obj))
          #t
          #f)
      #f))

(define (closure-params c)
  (if (closure? c)
      (second c)
      (error "~s is not a closure" c)))

(define (closure-body c)
  (if (closure? c)
      (third c)
      (error "~s is not a closure" c)))

(define (closure-env c)
  (if (closure? c)
      (fourth c)
      (error "~s is not a closure" c)))


;primitive operators
(define primitive-operators '(+ - * / add1 sub1 negate list cons car cdr eqv? lt? gt? leq? geq? null? list? number?))

;apply-proc 
(define apply-proc (lambda (proc args)
                     (cond
                       [(prim-proc? proc) (apply-primitive-op (prim-proc-symbol proc) args)]
                       [(closure? proc)(eval-exp (closure-body proc)(env (closure-params proc) args (closure-env proc)))]
                       [else (error 'apply-proc "Bad procedure: ~s" proc)])))
; prim-proc constructor
(define prim-proc (lambda (symbol)
                        (list 'prim-proc symbol)))

;prim-proc recognizer
(define prim-proc? (lambda (value)
                     (cond
                       [(not (list? value)) #f]
                       [else (eq? (first value) 'prim-proc)])))
;prim-proc accessor
(define prim-proc-symbol (lambda (value)
                           (second value)))
; primitive environment
(define prim-env
  (env primitive-operators
       (map prim-proc primitive-operators)
       empty-env))

;init environment
(define init-env
  (env '(x y null True False)
       '(23 42 '() True False)
       prim-env))

;apply primitive operators
(define apply-primitive-op (lambda (op args)
                             (cond
                               [(eq? op '+) (apply + args)]
                               [(eq? op '-) (apply - args)]
                               [(eq? op '*) (apply * args)]
                               [(eq? op '/) (apply / args)]
                               [(eq? op 'list) args]
                               [(eq? op 'cons) (apply cons args)]
                               [(eq? op 'car) (apply car args)]
                               [(eq? op 'cdr) (apply cdr args)]
                               [(eq? op 'add1) (apply add1 args)]
                               [(eq? op 'sub1) (apply sub1 args)]
                               [(eq? op 'negate) (apply (λ (num) (* num -1)) args)]
                               [(eq? op 'eqv?) (if (apply eqv? args) 'True 'False)]
                               [(eq? op 'lt?) (if (apply < args) 'True 'False)]
                               [(eq? op 'gt?) (if (apply > args) 'True 'False)]
                               [(eq? op 'leq?) (if (apply <= args) 'True 'False)]
                               [(eq? op 'geq?) (if (apply >= args) 'True 'False)]
                               [(eq? op 'eqv?) (if (apply eqv? args) 'True 'False)]
                               [(eq? op 'null?) (if (apply null? args) 'True 'False)]
                               [(eq? op 'list?) (if (apply list? args) 'True 'False)]
                               [(eq? op 'number?) (if (apply number? args) 'True 'False)]
                               [else (error 'apply-primitive-op "Bad operation: ~s" op)])))

