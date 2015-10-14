#lang racket
;; INTRODUCTION
;; I generate a population of finite state automata randomly
;; in each cycle, they are pair-matched to play a repeated game
;; their *fitness* is their relative payoff.
;; At the end of the cycle, I kill randomly 10%
;; and resurrect an equivalent amount to keep the population constant.
;; Automata are ressurrect based on their *fitness*.

;; Technically, the fitness vector is a vector that sums up to 1,
;; and we randomised over this vector to choose which automata to ressurrect
;; independently.

;; The game is a 2 player game,
;; each player has 2 possible stragies {Cooperate, Defect},
;; and they move simultaneously.
;; Here is the payoff matrix:
;;               Cooperate      Defect
;; Cooperate        3,3           0,4
;; Defect           4,0           1,1
;; This is a very familiar game in social science.
;; If both players "cooperate", they both get the reward of 3.
;; If one cooperates and the other defects, the cooperator is called "sucker"
;; because he gets 0 and the other gets 4. The payoff 4 is called "temptation".
;; If both defect, they both get the "punishment" payoff of 1.
;; This game captures the tradeoff between self interest (temptation)
;; and social welfare (both get reward).

(provide main)

;; ---------------------------------------------------------------------------------------------------
(require "evolution.rkt" "automata.rkt")

;(: simulation->lines (-> [Listof [List Real Real]]))
(define (simulation->lines)
  (define data (evolve (A) 1000 10 20))
  (define coors
    (for/list ;: [Listof [List Integer Real]]
      ([d ;: Payoff
        (in-list data)]
       [n ;: Integer
       (in-naturals)])
      (list n d)))
  coors)

;; the result is the population average:
;; how much an average automaton gets in the game in each cycle
;; if the average is 3, the society is in a prosperous period
;; in which all are cooperators
;; if the average gets down to 1, the society is in a state
;; of everybody defecting everybody
(define (main)
  (for ([i (in-range 4)])
    (simulation->lines))
  (void))

(time (main)) ;; 2000ms

;; ACKNOWLEDGEMENT
;; Thanks to the blog post of Tim Thornton,
;; http://timthornton.net/blog/id/538fa6f2f09a16ba0674813d
;; i know where to start
;; Thanks to Racket mailing list
;; https://groups.google.com/forum/#!topic/racket-users/4o1goSwrLHA
;; and IRC #racket for all the discussions
;; http://pastebin.com/sxrCnwRV
