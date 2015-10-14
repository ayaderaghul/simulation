#lang racket
(require "auto.rkt" "../match-claims.rkt")
(provide (all-defined-out))

;; previous-claim is a list of two claims
;; - the agent's own claim
;; - the opponent's claim
(define (next-claim automaton previous-claims)
  (let ([look-up
         (cond
          [(equal? previous-claims '(2 2)) automaton-hh]
          [(equal? previous-claims '(2 1)) automaton-hm]
          [(equal? previous-claims '(2 0)) automaton-hl]
          [(equal? previous-claims '(1 2)) automaton-mh]
          [(equal? previous-claims '(1 1)) automaton-mm]
          [(equal? previous-claims '(1 0)) automaton-ml]
          [(equal? previous-claims '(0 2)) automaton-lh]
          [(equal? previous-claims '(0 1)) automaton-lm]
          [(equal? previous-claims '(0 0)) automaton-ll])])
    (look-up automaton)))


(define (match-automaton-pair auto1 auto2 rounds-per-match)
  (define-values (round-result next1 next2)
    (for/fold ([round-result '()]
               [next1 (automaton-init-claim auto1)]
               [next2 (automaton-init-claim auto2)])
              ([i rounds-per-match])
      [define current-claims (list next1 next2)]
      [define next-claim1 (next-claim auto1 current-claims)]
      [define next-claim2 (next-claim auto2 (reverse current-claims))]
      [define result (match-claims current-claims)]
      (values (cons result round-result)
              next-claim1 next-claim2)))
  (reverse round-result))


