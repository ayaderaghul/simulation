#lang racket
(require "machine.rkt" "../match-claims.rkt")
(provide match-machine-pair)


(define (match-machine-pair auto1 auto2 rounds-per-match)
    (define round-results
      (for/fold ([round-result '()])
                ([i rounds-per-match])
        [define current-strat1 (machine-current-strat auto1)]
        [define current-strat2 (machine-current-strat auto2)]
        [define next-state1 (jump-to-state current-strat2 auto1)]
        [define next-state2 (jump-to-state current-strat1 auto2)]
        [define result (match-claims (list current-strat1 current-strat2))]
        (set! auto1 (update auto1 next-state1))
        (set! auto2 (update auto2 next-state2))
        (cons result round-result)))
    (reverse round-results))

