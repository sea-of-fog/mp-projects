#lang racket
(require "solution.rkt")
(require rackunit)

(define s (make-sim))
(define w1 (make-wire s))
(define w2 (make-wire s))
(define w3 (wire-and w1 w2))

(check-equal? (wire-value w1) #f)
(check-equal? (wire-value w2) #f)
(check-equal? (wire-value w3) #f)
(check-equal? (sim-time s) 0.0)

(wire-set! w1 #t)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #f)
(check-equal? (wire-value w3) #f)
(check-equal? (sim-time s) 0.0)

(sim-wait! s 0.5)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #f)
(check-equal? (wire-value w3) #f)
(check-equal? (sim-time s) 0.5)

(sim-wait! s 0.5)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #f)
(check-equal? (wire-value w3) #f)
(check-equal? (sim-time s) 1.0)

(wire-set! w2 #t)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #t)
(check-equal? (wire-value w3) #f)
(check-equal? (sim-time s) 1.0)

(sim-wait! s 0.9)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #t)
(check-equal? (wire-value w3) #f)
(check-equal? (sim-time s) 1.9)

(sim-wait! s 1.0)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #t)
(check-equal? (wire-value w3) #t)
(check-equal? (sim-time s) 2.9)