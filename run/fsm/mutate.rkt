#lang racket
(require "machine.rkt")

(provide (all-defined-out))

(define (set-immutable automaton posn new-value)
  (append
   (take automaton posn)
   (list new-value)
   (drop automaton (add1 posn))))

(define (mutate-machine an-auto)
  (let ([flatten-one (flatten-machine an-auto)]
        [r (random 41)]
        [c (random 3)]
	[s (random 10)])
    (if (member r (list 1 5 9 13 17 21 25 29 33 37))
        (make-machine (set-immutable flatten-one r c))
	(make-machine (set-immutable flatten-one r s))
        )))

