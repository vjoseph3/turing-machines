;; -------------------------------------------------------------------
;;   Joseph Vendryes
;;   January 2024
;;   Turing Machine Simulation
;; -------------------------------------------------------------------

#lang racket

;; This module provides an interface of functions to create and run
;;   deterministic 1-tape Turing machines on finite binary inputs.

;; The model of Turing machines used here includes the following
;;   components:
;;   - a language consisting of finitely many symbols, always
;;     including the symbols 0, 1, and blank;
;;   - a finite set of states, always including a starting state 1,
;;     an accepting state A, and a rejecting state R;
;;   - a tape, which is a string of consecutive spaces that extends
;;     infinitely in both directions, where each space holds exactly
;;     one symbol;
;;   - a tape head, always positioned over exactly one space of the
;;     tape, capable of reading and overwriting the symbol in the
;;     space over which it is positioned, and capable of moving one
;;     space to the right or to the left;
;;   - a movable indicator distinguishing exactly one state as
;;     being "current",
;;   - a transition function, which specifies, for every pair (a, s)
;;     where s is the current state and a is the symbol in the space
;;     under the tape head, the following three things:
;;     - a symbol with which to overwrite the space under the tape
;;       head,
;;     - a state to which to move the "current" indicator,
;;     - a direction (either rightward or leftward) in which to move
;;       the tape head one space over on the tape after completing
;;       the write operation.
;;
;; A Turing machine operates on a single input by following
;;   transitions from an initial configuration.
;; A configuration is the combination of a current state, a string of
;;   symbols represented on the tape, and a position of the tape head
;;   relative to the string of symbols on the tape. The following
;;   assumptions are made regarding every initial configuration:
;;   - the current state is 1;
;;   - only finitely many (possibly zero) spaces on the tape hold
;;     symbols other than blank;
;;   - no symbol other than 0, 1, or blank occurs anywhere on the
;;     tape;
;;   - no two spaces holding symbols other than blank have a blank
;;     symbol occurring in any of the spaces between them on the tape;
;;   - if there is a symbol other than blank, then the tape head is
;;     positioned over the leftmost space holding a symbol other than
;;     blank.
;;   The (possibly empty) string of symbols other than blank appearing
;;   on the tape in an initial configuration is called the input
;;   string or simply the input.
;; Following a transition means writing a symbol to the space under
;;   the tape head in the current configuration, setting a state
;;   as "current", and moving the tape head one space right or left,
;;   all as specified by the transition function given the current
;;   state and the symbol currently under the tape head.
;; The accepting state A and the rejecting state R are called the
;;   halting states. If a configuration is reached in which the
;;   current state is a halting state, then execution ceases:
;;   no further transitions are followed. In such a case, if the
;;   accepting state was current when execution ceased, then the
;;   Turing machine is said to have accepted the input. If the
;;   rejecting state was current, then the Turing machine is said to
;;   have rejected the input. In either case, the Turing machine is
;;   said to have halted on the input.

;; In representing Turing machines, their inputs, and their operation
;;   using Racket data types and functions, the following simplifying
;;   assumptions have been made (one may check that these assumptions
;;   do not materially impact the theoretical model):
;;   - the language of symbols consists of the blank symbol, _,
;;     together with a subset of the natural numbers:
;;     {_, 0, 1, ..., k} for some k greater than or equal to 1;
;;   - the set of states consists of the accepting and rejecting
;;     states together with a subset of the natural numbers:
;;     {1, ..., m, A, R} for some m greater than or equal to 1.


;; -------------------------------------------------------------------
;; Data Definitions

;; A Nat is an integer greater than or equal to 0

;; A Whole is an integer greater than or equal to 1

;; An AcceptingState (AS) is (anyof 'A 'R)

;; A State is (anyof Nat AS)

;; A Symbol is (anyof Whole '_)

;; A Move is (anyof 'r 'l)

;; A Step is a (step State Symbol Move)
(struct step (state symbol move))

;; A TuringMachine (TM) is a
;;   (tm Whole Whole (State Symbol -> Step))
;;   where whenever the transition function is given
;;     - a State that is a Nat <= tm-max-state and
;;     - a Symbol that is '_ or a Whole <= tm-max-symbol
;;   it produces a Step where
;;     - step-state is an AS or a Nat <= tm-max-state and
;;     - step-symbol is '_ or a Whole <= tm-max-symbol
(struct tm (max-state max-symbol transition))

;; A Config is a (config (listof Symbol) State (listof Symbol))
;; Usage notes:
;;   - config-prefix is a list of symbols on the tape starting with
;;     the space immediately to the left of the tape head and
;;     extending leftward at least far enough to include the leftmost
;;     symbol other than blank;
;;   - config-suffix is a list of symbols starting with the space
;;     under the tape head and extending rightward at least far
;;     enough to include the rightmost symbol other than blank.
(struct config (prefix state suffix))


;; -------------------------------------------------------------------

(provide
 (combine-out

  ;; see Data Definitions
  (struct-out step)
  (struct-out tm)
  (struct-out config)

  ;; TM Config -> Config
  ;; (simulate-step tm before)
  ;; produces the configuration of tm after following one transition
  ;;   from the configuration before
  ;; Requires:
  ;;   - every Symbol in before-prefix and before-suffix is either
  ;;     '_ or a Whole <= tm-max-symbol;
  ;;   - before-state is a Nat <= tm-max-state.
  simulate-step

  ;; TM (listof Symbol) -> Config
  ;; (simulate tm input)
  ;; produces the halting configuration of tm when given input
  ;; Requires:
  ;;   - every Symbol in input is either '_ or
  ;;     a Whole <= tm-max-symbol;
  ;;   - tm halts on input.
  simulate

  ;; Config -> (listof Symbol)
  ;; (current-symlist conf)
  ;; produces the current list of symbols in the configuration conf
  ;;   including symbols before and after the tape head,
  ;;   possibly with leading and trailing '_ symbols
  current-symlist

  ))
;; *******************************************************************
;; *******************                             *******************
;; *******************   END OF PUBLIC INTERFACE   *******************
;; *******************                             *******************
;; *******************************************************************


;; ---------------
;; see interface
(define (simulate-step tm before)
  (define the-step ((tm-transition tm) (config-state before)
                                       (current-symbol before)))
  (config
   (match (step-move the-step)
     ['r (cons (step-symbol the-step) (config-prefix before))]
     ['l (symlist-rest (config-prefix before))])
   (step-state the-step)
   (match (step-move the-step)
     ['r (symlist-rest (config-suffix before))]
     ['l (cons (symlist-first (config-prefix before))
               (cons (step-symbol the-step)
                     (symlist-rest (config-suffix before))))])))

;; ---------------
;; Config -> Symbol
;; produces the symbol under the tape head in the configuration conf
(define (current-symbol conf)
  (symlist-first (config-suffix conf)))

;; ---------------
;; (listof Symbol) -> Symbol
;; produces the first symbol in symlist, extending with '_ if empty
(define (symlist-first symlist)
  (if (empty? symlist)
      '_
      (first symlist)))

;; ---------------
;; (listof Symbol) -> (listof Symbol)
;; produces the rest of symlist, extending with '_ if empty
(define (symlist-rest symlist)
  (if (empty? symlist)
      empty
      (rest symlist)))

;; ---------------
;; see interface
(define (simulate tm input)
  (simulate/config tm (config empty 1 input)))

;; ---------------
;; TM Config -> Config
;; produces the halting configuration of tm when run from the given
;;   initial configuration
;; Requires:
;;   - every Symbol in initial-prefix and initial-suffix is either
;;     '_ or a Whole <= tm-max-symbol;
;;   - initial-state is either an AS or a Nat <= tm-max-state;
;;   - tm halts when run from initial.
(define (simulate/config tm initial)
  (if (accepting-state? (config-state initial))
      initial
      (simulate/config tm (simulate-step tm initial))))

;; ---------------
;; State -> Bool
;; produces true iff state is an AS
(define (accepting-state? state)
  (not (integer? state)))

;; ---------------
;; see interface
(define (current-symlist conf)
  (append (reverse (config-prefix conf))
          (config-suffix conf)))
