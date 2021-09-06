#lang racket
;Marilyn Recarte

(require racket/stream)
(require "keyboard.rkt")

(provide (all-defined-out))

(define test-stream
  (stream-cons 'x (stream-cons 'y (stream-cons 'z test-stream))))

;1

(define stream-remove-all (lambda (x s)
                      (cond
                        [(eq? (stream-first s) x) (stream-remove-all x (stream-rest s))]
                        [else (stream-cons (stream-first s) (stream-remove-all x (stream-rest s)))])))

;2
                  
(define stream-replace (lambda (x y s)
                     (cond
                       [(eq? (stream-first s) x) (stream-cons y (stream-replace x y (stream-rest s)))]
                       [else (stream-cons (stream-first s) (stream-replace x y (stream-rest s)))])))


;3

(define (pairs-from p)
  (stream-cons p (pairs-from (next-pair p))))

(define all-pairs (pairs-from (cons 1 1)))

(define next-pair
    (lambda (p)
        (let ([x (car p)] [y (cdr p)])
          (cond
            [(= y 1) (cons 1 (+ x y))]
            [else (cons (+ x 1) (- y 1))]))))

;4

(define stream-merge (lambda (s1 s2)
                 (cond
                   [(= (stream-first s1) (stream-first s2)) (stream-merge s1 (stream-rest s2))]
                   [(< (stream-first s1) (stream-first s2)) (stream-cons (stream-first s1) (stream-merge (stream-rest s1) s2))]
                   [else (stream-cons (stream-first s2) (stream-merge s1 (stream-rest s2)))])))
                         
(define ham (stream-cons 1 (stream-merge
                             (stream-map (λ (n) (* 2 n)) ham)
                             (stream-merge (stream-map (λ (n) (* 3 n)) ham)
                                           (stream-map (λ (n) (* 5 n)) ham)))))

;5

(define (stream-add s t)
  (cond [(stream-empty? s) empty-stream]
        [(stream-empty? t) empty-stream]
        [else
         (stream-cons (+ (stream-first s)
                         (stream-first t))
                      (stream-add (stream-rest s)
                                  (stream-rest t)))]))
(define (stream-mul s t)
  (cond [(stream-empty? s) empty-stream]
        [(stream-empty? t) empty-stream]
        [else
         (stream-cons (* (stream-first s)
                         (stream-first t))
                      (stream-mul (stream-rest s)
                                  (stream-rest t)))]))

(define (partial-sums s)
  (cond [(stream-empty? s) empty-stream]
        [else
         (letrec ([sums (stream-add s (stream-cons 0 sums))])
           sums)]))

(define (integers-from n)
  (stream-cons n (integers-from (add1 n))))

(define ints (integers-from 0))
(define evens (stream-map (λ (n) (* 2 n)) ints))
(define ones (stream-cons 1 ones))
(define odds (stream-add ones evens))

(define fact-stream
  (stream-cons 1 (stream-mul fact-stream (integers-from 1))))

(define powers (lambda(x) (stream-cons 1 (stream-map (lambda (t) (* x t)) (powers x)))))

(define e-coeffs
  (stream-map (λ (n) (/ 1.0 n)) fact-stream))

(define (e-approx x)
  (partial-sums (stream-mul (powers x) e-coeffs)))

(define helper (lambda (s1 s2) (stream-cons (* (stream-first s1) (stream-first s2)) (helper (stream-rest s1) (stream-rest s2)))))

(define alternating1
  (stream-cons 0 (stream-cons 1 (stream-cons 0 (stream-cons -1 alternating1)))))

(define alternating2
  (stream-cons 1 (stream-cons 0 (stream-cons -1 (stream-cons 0 alternating2)))))

(define sin-coeffs
  (helper alternating1 e-coeffs))

(define cos-coeffs
  (helper alternating2 e-coeffs))

;6

(define (sin-approx x)
  (partial-sums (stream-mul (powers x) sin-coeffs)))

(define (cos-approx x)
  (partial-sums (stream-mul (powers x) cos-coeffs)))

;7

(define (grune-a-b s)
  (cond [(stream-empty? s) empty-stream]
    [(stream-empty? (stream-rest s)) s]
    [(and (equal? (stream-first s) 'a) (equal? 'a (stream-first (stream-rest s))))
     (stream-cons 'b (grune-a-b (stream-rest (stream-rest s))))]
    [else (stream-cons (stream-first s) (grune-a-b (stream-rest s)))]))

;8

(define (grune a b)
  (lambda (s)
    (cond [(stream-empty? s) empty-stream]
          [(stream-empty? (stream-rest s)) s]
          [(and (equal? (stream-first s) a) (equal? a (stream-first (stream-rest s))))
           (stream-cons b ((grune a b) (stream-rest (stream-rest s))))]
          [else (stream-cons (stream-first s) ((grune a b) (stream-rest s)))])))
                    
