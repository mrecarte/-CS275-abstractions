#lang racket

(provide (all-defined-out))

;parse function
(define (parse input)
  (define (parse-error)
    (error 'parse "Invalid syntax ~s" input))
  (cond
    [(number? input) (lit-exp input)]
    [(symbol? input) (var-exp input)]
    [(list? input)
     (cond [(empty? input) (parse-error)]
           [(eq? (first input) 'if)
            (if (= (length input) 4)
                (ite-exp (parse (second input)) (parse (third input)) (parse (fourth input)))
                (parse-error))]
           [(eq? (first input) 'let)
            (if (= (length input) 3)
                (let ([binding-list (second input)])
                  (let-exp (map first binding-list) (map parse (map second binding-list))(parse (third input))))
                (parse-error))]
           [(eq? (first input) 'lambda)
                (if (= (length input) 3)
                    (lambda-exp (second input)
                                (parse (third input)))
                    (parse-error))]
           [(eq? (first input) 'set!)
                (if (= (length input) 3)
                    (set-exp (second input)
                             (parse (third input)))
                    (parse-error))]
           [(eq? (first input) 'begin)
                (begin-exp (map parse (rest input)))]
           [(eq? (first input) 'letrec)
                (if (= (length input) 3)
                    (parse-letrec input)
                    (parse-error))]
           [else (app-exp (parse (first input))
                          (map parse (rest input)))])]
    [else (parse-error)]))

;------------------------------------------------------------------------------------------------------------------
;MINISCHEME A
;lit-exp constructor
(define lit-exp (lambda (input)
                  (list 'lit-exp input)))

;lit-exp recognizer
(define lit-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (first input) 'lit-exp)])))

;lit-exp accessor
(define lit-exp-num (lambda (input)
                      (if (lit-exp? input)(second input)
                          (error 'lit-exp-num "not a lit-exp expression ~s" input))))

;------------------------------------------------------------------------------------------------------------------
;MINISCHEME B
; var-exp constructor
(define var-exp (lambda (input)
                  (list 'var-exp input)))

; var-exp recognizer
(define var-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (first input) 'var-exp)])))

;var-exp accessor
(define var-exp-symbol (lambda (input)
                         (if (var-exp? input)(second input)
                             (error 'var-exp-symbol "Not app-exp expression ~s" input))))

;------------------------------------------------------------------------------------------------------------------
;MINISCHEME C
; app-exp constructor
(define app-exp (lambda (proc args)
                  (list 'app-exp proc args)))

; app-exp recognizer
(define app-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (first input) 'app-exp)])))

; app-exp procedure accessor
(define app-exp-proc (lambda (app)
                       (if (app-exp? app) (second app)
                           (error 'app-exp-proc "not a app-exp ~s" app))))

; app-exp arguments accessor
(define app-exp-args (lambda (app)
                       (if (app-exp? app)(third app)
                           (error 'app-exp-args "not a app-exp ~s" app))))

;------------------------------------------------------------------------------------------------------------------
;MINISCHEME D
; ite-exp constructor
(define ite-exp (lambda (ite-cond ite-then ite-else)
                  (list 'ite-exp ite-cond ite-then ite-else)))

; if-exp recognizer
(define ite-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (first input) 'ite-exp)])))

; if-exp cond accessor
(define ite-exp-cond (lambda (input)
                       (if (ite-exp? input)(second input)
                           (error 'ite-exp-cond "not a ite-exp ~s" input))))

; if-exp then accessor
(define ite-exp-then (lambda (input)
                       (if (ite-exp? input)(third input)
                           (error 'ite-exp-then "not a ite-exp ~s" input))))

; if-exp else accesssor
(define ite-exp-else (lambda (input)
                       (if (ite-exp? input)(fourth input)
                           (error 'ite-exp-else "not an ite-exp ~s" input))))

;------------------------------------------------------------------------------------------------------------------
;MINISCHEME E
;let-exp constructor
(define let-exp (lambda (symbols expression body)
                  (list 'let-exp symbols expression body)))

;let-exp recognizer
(define let-exp? (lambda (input)
                   (cond
                     [(not (list? input)) #f]
                     [else (eq? (first input) 'let-exp)])))

; let-exp symbols accessor
(define let-exp-symbols (lambda (input)
                          (if (let-exp? input) (second input)
                              (error 'let-exp-symbols "Not a let expression ~s" input))))

; let-exp values accessor
(define let-exp-vals (lambda (input)
                       (if (let-exp? input)(third input)
                           (error 'let-exp-vals "Not a let expression ~s" input))))

; let-exp body accessor
(define let-exp-body (lambda (input)
                       (if (let-exp? input)(fourth input)
                           (error 'let-exp-body "Not a let expression ~s" input))))

;------------------------------------------------------------------------------------------------------------------
;MINISCHEME F
;lambda-exp constructor
(define lambda-exp
  (lambda (params exp)
    (list 'lambda-exp params exp)))

;lambda-exp recognizer
(define lambda-exp?
  (lambda (input)
    (cond
      [(list? input) (eq? 'lambda-exp (first input))]
      [else (error 'identifier "Bad input: ~s" input)])))

;lamd-exp params accessor
(define lambda-exp-params
  (lambda (input)
    (cond
      [(lambda-exp? input) (second input)]
      [else (error 'get "Bad input: ~s" input)])))

;lambda-exp body accessor
(define lambda-exp-body
  (lambda (input)
    (cond
      [(lambda-exp? input) (third input)]
      [else (error 'get "Bad input: ~s" input)])))

;------------------------------------------------------------------------------------------------------------------
;MINISCHEME G

(define set-exp
  (lambda (symbol exp)
    (list 'set-exp symbol exp)))


(define (set-exp? input)
  (if (equal? (first input) 'set-exp) #t #f))

(define set-exp-symbol
  (lambda (input)
    (cond
      [(set-exp? input) (second input)]
      [else (error 'getter "Bad input: ~s" input)])))

(define set-exp-value
  (lambda (input)
    (cond
      [(set-exp? input) (third input)]
      [else (error 'getter "Bad input: ~s" input)])))

(define begin-exp
  (lambda (listofexps)
    (list 'begin-exp listofexps)))

(define begin-exp?
  (lambda (input)
    (cond
      [(list? input) (eq? 'begin-exp (first input))]
      [else (error 'identifier "Bad input: ~s" input)])))

(define get-begin-exps
  (lambda (input)
    (cond
      [(begin-exp? input) (second input)]
      [else (error 'identifier "Bad input: ~s" input)])))

;------------------------------------------------------------------------------------------------------------------
;MINISCHEME H

(define (parse-letrec input)
  (let* ([syms (map first (second input))]
         [exps (map second (second input))]
         [body (third input)]
         [new-syms (map (λ (s) (gensym)) syms)])
    (let-exp syms
             (map (λ (s) (lit-exp 0)) syms)
             (let-exp new-syms
                      (map parse exps)
                      (begin-exp
                        (append (map
                                 (λ (s new-s)
                                   (set-exp s (var-exp new-s)))
                                 syms new-syms)
                                (list (parse body))))))))