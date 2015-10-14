#lang racket

(provide (all-defined-out))
(require "./ibar/match-pair.rkt" "./fsm/match-pair.rkt")

;; in each match, take mean of round results for each automaton
;; returns a pair of means
(define (take-sums round-results)
  (map (lambda (f) (apply +  (map f round-results)))
       (list first second)))


(define (take-delta* f round-results delta)
  (let ([first-auto (map f round-results)])
    (for/list ([i (length first-auto)])
      (* (expt delta i) (list-ref first-auto i)))))

(define (take-delta round-results delta)
  (map (lambda (x) (apply + (take-delta* x round-results delta)))
       (list first second)))


(define (match-population what-type population rounds-per-match delta)
  (define population-result
    (for/fold ([population-result '()])
              ([i (/ (length population) 2)])
      [define round-result
        ((if (equal? what-type "ibar") match-automaton-pair match-machine-pair)
         (list-ref population (* i 2))
         (list-ref population (add1 (* i 2)))
         rounds-per-match)]
      (cons
       (take-delta round-result delta)
       population-result)))
  (flatten (reverse population-result)))

