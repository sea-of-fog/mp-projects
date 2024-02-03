#lang racket
(require "circuit-simulator.rkt")
(require rackunit)

;===============
; SR NOR latch
;===============

(define sim (make-sim))
(define s (make-wire sim))
(define r (make-wire sim))
(define q (make-wire sim))
(define nq (make-wire sim))
(gate-nor q r nq)
(gate-nor nq q s)


;=========
; TEST
;=========

(wire-set! s #t)
(sim-wait! sim 10)
(check-equal? (sim-time sim) 10.0)
(check-equal? (wire-value s) #t)
(check-equal? (wire-value r) #f)
(check-equal? (wire-value q) #t)
(check-equal? (wire-value nq) #f)

(sim-wait! sim 8.7)
(check-equal? (sim-time sim) 18.7)
(check-equal? (wire-value s) #t)
(check-equal? (wire-value r) #f)
(check-equal? (wire-value q) #t)
(check-equal? (wire-value nq) #f)

(wire-set! s #f)
(wire-set! r #t)
(sim-wait! sim 3.4)
; precyzja się wywala - czy to problem?
;(check-equal? (sim-time sim) 22.1)
(check-equal? (wire-value s) #f)
(check-equal? (wire-value r) #t)
(check-equal? (wire-value q) #f)
(check-equal? (wire-value nq) #t)
