#lang racket

(require "auto.rkt" "../main.rkt")
(provide (all-defined-out))
;; test 2

(define (create-test-population high medium low accom)
   (shuffle
   (append
    (make-list high all-highs)
    (make-list medium all-mediums)
    (make-list low all-lows)
    (make-list accom accommodator))))


;; TEST 2
(define (run-test2 cycles h m l a d name-list)
  [define B (create-test-population h m l a)]
  (time (evolve B cycles 10 0 30 d name-list)))

(define test-points
  (list
   (list 900 50 1 49)
   (list 700 250 1 49)
   (list 400 550 1 49)
   (list 50 50 450 450)
   (list 250 250 250 250)
   (list 400 200 200 200)))

(define (run-many-test2 points)
  (for ([i (length points)])
    [define p (list-ref points i)]
    [define B (create-test-population (first p)
                                      (second p)
                                      (third p)
                                      (last p))]
    (time (evolve B 5000 10 0 30 1 (list
                                    "R:/ibar/test2p"
                                    (number->string i)
                                    ".txt")))))



;; test 1s



(define (random-one-shot-population
         h-n-types m-n-types l-n-types)
  (shuffle
   (append
    (random-population*
     1 (for/list ([l l-n-types])
         (number->automaton (random 19683))))
    (random-population*
     1 (for/list ([m m-n-types])
         (number->automaton (+ 19683 (random 19683)))))
    (random-population*
     1 (for/list ([h h-n-types])
         (number->automaton (+ 39366 (random 19683))))))))


;; TEST 1
(define (run-many-oneshot points )
  (for ([i (length points)])
    [define p (list-ref points i)]
    [define B (random-one-shot-population (last p) (second p) (first p))]
    (time (evolve B 1000 10 0 1 1
                  (list (string-append
                         "R:/ibar/test1p"
                         (number->string i)
                         ".txt"))))))


(define point-list
  (list
  (list 900 50 50)
  (list 800 50 150)
  (list 50 50 900)
  (list 50 150 800)
  (list 800 150 50)
  (list 700 250 50)))

