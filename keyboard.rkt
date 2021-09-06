#lang racket

(require racket/stream)
(provide keyboard-stream output-stream)

; (keyboard-stream) is a stream of s-expressions typed by the user.
(define (keyboard-stream)
  (display "? ")
  (let ([this (read)])
    (if (eof-object? this)
        empty-stream
        (stream-cons this (keyboard-stream)))))

(define (output-stream s)
  (cond [(stream-empty? s) (void)]
        [else
         (displayln (stream-first s))
         (output-stream (stream-rest s))]))
