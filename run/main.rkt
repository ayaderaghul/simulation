#lang racket
(provide (all-defined-out))

(require "./ibar/auto.rkt"
         "./fsm/machine.rkt"
         "match-population.rkt"
         "fit.rkt"
         "mutate-population.rkt"
         "scan.rkt"
         "tv.rkt"
         "in.rkt"
         "out.rkt")


(define (evolve what-type population cycles speed mutation rounds-per-match delta file-list)
  (define N (length population))
  (define-values (result popu
                        ; 2-types
                         )
    (for/fold ([result '()]
               [population population]
              ; [2-types '()]
               )
              ([i cycles])
     ; [define types (scan-4-types population)]
      [define round-results (match-population what-type population rounds-per-match delta)]
      [define total (apply + (flatten round-results))]
      [define max-payoff (apply max (flatten round-results))]
      [define average-payoff (exact->inexact
                              (/ total N))]
      [define accum-fitness (accumulated-payoff-percentages (flatten round-results))]
      [define survivors (drop population speed)]
      [define successors
        (randomise-over-fitness accum-fitness population speed)]
      [define before-mutation (shuffle (append survivors successors))]
      [define new-population (mutate-populations
                              what-type
                              mutation
                              before-mutation)]
      (out-rank i new-population 6 (second file-list))
      (values (cons average-payoff result)
              new-population
              ;(cons types 2-types)
              )))
  (out-mean (reverse result) (first file-list))
  (plot-mean (reverse result) (third file-list))
 ; (plot-dynamic (reverse 2-types) N)
 ; (out-mean (flatten (reverse 2-types)) (first file-list))
  )

(define (run-one what-type)
  [define B (if (equal? what-type "ibar")
                (random-population 1 100)
                (create-population 100))]
  [define name-list (list "m" "r" "p.png")]
  (time (evolve what-type B 100 10 1 3 .3 name-list)))

(define (run-many what-type)
  (for* ([i (in-list speed-list)]
         [j (in-list round-list)]
	[k (in-list delta-list)])
    [define B (cond
               [(equal? what-type "ibar") (random-population 1 100)]
               [(equal? what-type "fsm") (create-population 100)])]
    [define name-list (n->srd what-type i j k)]
    (time (evolve what-type B 500000 i 1 j k name-list))))

(define speed-list
  (list 1 5 10 20))
(define round-list
  (list 1 10 20 50))
(define delta-list
  (list 0 .2 .8 .9 1))
