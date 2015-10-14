#lang racket

(provide (all-defined-out))

;; AUTOMATON
(struct state (name result0 result1 result2) #:transparent)
;; a state: name and many transition rules
(struct machine (current-state states) #:transparent)
;; the machine itself: current state + states
;; mutable data structure seems very hard to handle later on

(define (machine-current-strat an-auto)
  (state-name (list-ref
               (machine-states an-auto)
               (machine-current-state an-auto))))

(define (create-machine* init-state
                           state0 state1 state2 state3 state4
                           state5 state6 state7 state8 state9
                           result00 result01 result02
                           result10 result11 result12
                           result20 result21 result22
                           result30 result31 result32
                           result40 result41 result42
                           result50 result51 result52
                           result60 result61 result62
                           result70 result71 result72
                           result80 result81 result82
                           result90 result91 result92)
  (machine init-state
             (list (state state0 result00 result01 result02)
                   (state state1 result10 result11 result12)
                   (state state2 result20 result21 result22)
                   (state state3 result30 result31 result32)
                   (state state4 result40 result41 result42)
                   (state state5 result50 result51 result52)
                   (state state6 result60 result61 result62)
                   (state state7 result70 result71 result72)
                   (state state8 result80 result81 result82)
                   (state state9 result90 result91 result92)
                   )))

(define (create-machine)
  (create-machine* (random 10) ;; number 10 here is 10 states
                     (random 3) (random 3) (random 3) (random 3) (random 3)
                     (random 3) (random 3) (random 3) (random 3) (random 3)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                     (random 10) (random 10) (random 10)
                                        ))


;; AUTOMATON BEHAVIOR
(define (jump-to-state an-event an-auto)
  (define result-state (list-ref
                        (machine-states an-auto)
                        (machine-current-state an-auto)))
  (cond [(= an-event 0) (state-result0 result-state)]
        [(= an-event 1) (state-result1 result-state)]
        [(= an-event 2) (state-result2 result-state)]))

(define (react an-event an-auto)
  (define states (machine-states an-auto))
  (define next-state-id (jump-to-state an-event an-auto))
  (state-name (list-ref states next-state-id)))

(define (update old-auto new-state)
  (struct-copy machine old-auto
               [current-state new-state]))


(define (flatten-state a-state)
  (map (lambda (f) (f a-state))
       (list
        state-name
        state-result0
        state-result1
        state-result2)))

(define (flatten-machine an-auto)
  (flatten
   (append
    (list (machine-current-state an-auto))
    (map flatten-state (machine-states an-auto)))))
(define (make-machine a-list)
  (machine (first a-list)
             (list (apply state (take (drop a-list 1) 4))
                   (apply state (take (drop a-list 5) 4))
                   (apply state (take (drop a-list 9) 4))
                   (apply state (take (drop a-list 13) 4))
                   (apply state (take (drop a-list 17) 4))
                   (apply state (take (drop a-list 21) 4))
                   (apply state (take (drop a-list 25) 4))
                   (apply state (take (drop a-list 29) 4))
                   (apply state (take (drop a-list 33) 4))
                   (apply state (take-right a-list 4)))))

(define (state-labels automaton)
  (map (lambda (a-state)
         (state-name a-state))
       (machine-states automaton)))




;; generate population
(define (create-population N)
  (for/list
      ([n N])
    (create-machine)))

