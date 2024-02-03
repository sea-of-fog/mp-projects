#lang racket
(require "solution.rkt")
(require rackunit)

; prosty test - sprawdza działanie anda

(define s (make-sim))
(define w1 (make-wire s))
(define w2 (make-wire s))
(define w3 (wire-and w1 w2))

(check-equal? (wire-value w1) #f)
(check-equal? (wire-value w2) #f)
(check-equal? (wire-value w3) #f)

(wire-set! w1 #t)
(wire-set! w2 #t)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #t)
(check-equal? (wire-value w3) #f)

(sim-wait! s 0.5)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #t)
(check-equal? (wire-value w3) #f)

(sim-wait! s 0.5)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #t)
(check-equal? (wire-value w3) #t)

(sim-wait! s 10)

(check-equal? (wire-value w1) #t)
(check-equal? (wire-value w2) #t)
(check-equal? (wire-value w3) #t)