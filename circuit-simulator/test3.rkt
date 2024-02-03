#lang racket
(require "solution.rkt")
(require rackunit)

; test to see if the algorithm loops

(define s (make-sim))
(define w (make-wire s))
(gate-not w w)

(check-equal? (sim-time s) 0.0)
(check-equal? (wire-value w) #f)

; even though there is a gate, nothing has changed yet

(sim-wait! s 1.2)

(check-equal? (sim-time s) 1.2)
(check-equal? (wire-value w) #t)

(sim-wait! s 0.8)
(check-equal? (sim-time s) 2.0)
(check-equal? (wire-value w) #f)

(wire-set! w #t)
(check-equal? (sim-time s) 2.0)
(check-equal? (wire-value w) #t)

(sim-wait! s 0.5)
(check-equal? (sim-time s) 2.5)
(check-equal? (wire-value w) #t)

(sim-wait! s 0.5)
(check-equal? (sim-time s) 3.0)
(check-equal? (wire-value w) #t)