#lang racket
(provide (all-defined-out))
(require "./ibar/mutate.rkt" "./fsm/mutate.rkt")

(define (mutate-populations what-type mutation population)
  [define l (length population)]
  [define-values (popu)
    (for/fold ([popu population])
              ([i (in-range mutation)])
      [define r (random l)]
      [define mutated
        ((if (equal? what-type "ibar") mutate-automaton mutate-machine)
                (list-ref population r))]
      [define new-population (set-immutable population r mutated)]
      (values new-population))]
  popu)
