#lang racket

(provide (all-defined-out))


(struct automaton (init-claim hh hm hl mh mm ml lh lm ll) #:transparent)
(define accommodator (automaton 1 0 1 2 0 1 2 0 1 2))
(define all-highs (automaton 2 2 2 2 2 2 2 2 2 2))
(define all-mediums (automaton 1 1 1 1 1 1 1 1 1 1))
(define all-lows (automaton 0 0 0 0 0 0 0 0 0 0))

(define (identify automaton)
  (drop (vector->list (struct->vector automaton)) 1))

(define (all-highs? automaton)
  (equal? automaton all-highs))
(define (all-mediums? automaton)
  (equal? automaton all-mediums))
(define (all-lows? automaton)
  (equal? automaton all-lows))
(define (accommodator? automaton)
  (equal? automaton accommodator))
(define (identify-2-types population)
  (list
   (count all-highs? population)
   (count all-mediums? population)
   ))


;; mass production
(define (base10->base3 n)
  (~r n #:base 3 #:min-width 10 #:pad-string "0"))

(define (char->digit c)
  (case c
    ;; (map (lambda (i) (format "[(#\\~a) ~a]" i i))
    ;;      (range 0 10))
    [(#\0) 0]
    [(#\1) 1]
    [(#\2) 2]
    [(#\3) 3]
    [(#\4) 4]
    [(#\5) 5]
    [(#\6) 6]
    [(#\7) 7]
    [(#\8) 8]
    [(#\9) 9]))

(define (base3->digits a-string)
  (map char->digit (string->list a-string)))

(define (number->automaton n)
  (apply automaton (base3->digits (base10->base3 n))))
(define (automaton->number automaton)
  (string->number
   (apply string-append (map number->string automaton))
   3))

;; create population
(define (random-population* n-automata-per-type types)
  (shuffle
   (flatten
    (for/list ([i types])
      (make-list n-automata-per-type i)))))

(define (random-population
         n-automata-per-type n-types)
  (random-population*
   n-automata-per-type
   (for/list ([i n-types])
     (number->automaton (random 59049)))))
