#lang racket

(require "auto.rkt" "main.rkt")

(provide (all-defined-out))

;; create test population

(define (create-test-population l m h)
  (shuffle
   (append
    (for/list
        ([n l])
      (generate-lows))
    (for/list
        ([n m])
      (generate-mediums))
    (for/list
        ([n h])
      (generate-highs)))))

(define (generate-lows)
  (make-automaton
   (list 0
         0 (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5))))


(define (generate-mediums)
  (make-automaton
   (list 0
         1 (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5))))


(define (generate-highs)
  (make-automaton
   (list 0
         2 (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5)
         (random 3) (random 5) (random 5) (random 5))))



(define (run-many-oneshot points d)
  (for ([i (length points)])
    [define p (list-ref points i)]
    [define B (random-one-shot-population (last p) (second p) (first p))]
    (time (evolve B 1000 10 0 1 d
                  (list (string-append
                         "R:/fsm/1s0d"
                         (string-trim (number->string (* 10 d)) ".0")
                         "p"
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
