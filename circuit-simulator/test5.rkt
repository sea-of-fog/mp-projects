#lang racket
(require "circuit-simulator.rkt")
(require rackunit)

(define sim (make-sim))

(define (make-counter n clk en)
  (if (= n 0)
      '()
      (let [(out (make-wire sim))]
        (flip-flop out clk (wire-xor en out))
        (cons out (make-counter (- n 1) clk (wire-and en out))))))

(define clk (make-wire sim))
(define en  (make-wire sim))
(define counter (make-counter 4 clk en))

(wire-set! en #t)

; Kolejne wywołania funkcji tick zwracają wartość licznika
; w kolejnych cyklach zegara. Licznik nie jest resetowany,
; więc początkowa wartość licznika jest trudna do określenia
(define (tick)
  (wire-set! clk #t)
  (sim-wait! sim 5)
  (wire-set! clk #f)
  (sim-wait! sim 5)
  (bus-value counter))

(define cnt (tick))
(define (mod16 n)
  (if (>= n 16)
      (- n 16)
      n))

(check-equal? (tick)
              (mod16 (+ cnt 1)))

(check-equal? (tick)
              (mod16 (+ cnt 2)))

(check-equal? (tick)
              (mod16 (+ cnt 3)))

(check-equal? (tick)
              (mod16 (+ cnt 4)))

(check-equal? (tick)
              (mod16 (+ cnt 5)))

(check-equal? (tick)
              (mod16 (+ cnt 6)))

(check-equal? (tick)
              (mod16 (+ cnt 7)))

(check-equal? (tick)
              (mod16 (+ cnt 8)))

(check-equal? (tick)
              (mod16 (+ cnt 9)))

(check-equal? (tick)
              (mod16 (+ cnt 10)))

(check-equal? (tick)
              (mod16 (+ cnt 11)))

(check-equal? (tick)
              (mod16 (+ cnt 12)))

(check-equal? (tick)
              (mod16 (+ cnt 13)))

(check-equal? (tick)
              (mod16 (+ cnt 14)))

(check-equal? (tick)
              (mod16 (+ cnt 15)))
