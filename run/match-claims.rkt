#lang racket
(provide (all-defined-out))

(define (match-claims claims)
  (if (<= (apply + claims) 2)
      (map convert-payoff claims)
      (list 0 0)))

(define (convert-payoff x)
  (cond [(= x 0) 2]
        [(= x 1) 5]
        [(= x 2) 8]))
