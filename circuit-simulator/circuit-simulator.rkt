#lang racket
(require data/heap)

(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]))

; action to 0-argumentowa lambda, która zwraca void
; czy lepiej, żeby acts to była mutowalna lista, czy nie?
; TOOD: tylko ostatnie pole mutowalne w wire
; TODO: czy gate-y są potrzene?
(struct wire (value sim acts) #:mutable)
(struct sim (events time) #:mutable)
(define-struct event (action time))
; nie ma struktury na akcje - akcje to lambdy 0-argumentowe

(define (event-comp e1 e2)
                     (<= (event-time e1) (event-time e2)))

(define (make-sim)
  (sim (make-heap event-comp) 0.0))

(define (heap-empty? h)
  (= 0 (heap-count h)))

(define (sim-wait-until! s tgt)
  (if (heap-empty? (sim-events s))
      (set-sim-time! s tgt)
      (let ([e (heap-min (sim-events s))])
        (if (> (event-time e) tgt)
            (set-sim-time! s tgt)
            (begin
              (set-sim-time! s (event-time e))
              ((event-action e))
              (heap-remove-min! (sim-events s))
              (sim-wait-until! s tgt))))))         

(define (sim-wait! s t)
  (sim-wait-until! s (+ t (sim-time s))))

(define (sim-add-action! s t act)
  (heap-add! (sim-events s) (event act (+ (sim-time s) t))))

(define (make-wire s)
  (wire #f s null))

(define (wire-on-change! w act)
  (act)
  (set-wire-acts! w (cons act (wire-acts w))))

; TODO: przepisać na match
(define (activate-actions acts)
  (if (null? acts)
      (void)
      (begin ((car acts))
             (activate-actions (cdr acts)))))

(define (wire-trigger w)
  (activate-actions (wire-acts w)))

(define (wire-set! w b)
  (if (eq? b (wire-value w))
      (void)
      (begin (set-wire-value! w b)
             (wire-trigger w))))

;==============================================
; Bramki
;==============================================

; TODO: przepisać na match
; czy traktowanie nota jako "podwójnego wejścia" to problem
(define (gate-recalc-action type delay in1 in2 out)
    (cond [(eq? type 'not)  (lambda () (wire-set! out (not (wire-value in1))))]
          [(eq? type 'and)  (lambda () (wire-set! out (and (wire-value in1)
                                                           (wire-value in2))))]
          [(eq? type 'nand) (lambda () (wire-set! out (not (and (wire-value in1)
                                                                (wire-value in2)))))]
          [(eq? type 'or)   (lambda () (wire-set! out (or (wire-value in1)
                                                          (wire-value in2))))]
          [(eq? type 'nor)  (lambda () (wire-set! out (not (or (wire-value in1)
                                                          (wire-value in2)))))]
          [(eq? type 'xor)  (lambda () (wire-set! out (and (or (wire-value in1)
                                                               (wire-value in2))
                                                           (not (and (wire-value in1)
                                                                     (wire-value in2))))))]))

(define (gate-action type delay in1 in2 out)
         (lambda () (sim-add-action! (wire-sim in1) delay (gate-recalc-action type delay in1 in2 out))))

(define (gate-not out in)
  (wire-on-change! in (gate-action 'not 1 in in out)))

(define (gate-and out in1 in2)
  (wire-on-change! in1 (gate-action 'and 1 in1 in2 out))
  (wire-on-change! in2 (gate-action 'and 1 in1 in2 out)))

(define (gate-nand out in1 in2)
  (wire-on-change! in1 (gate-action 'nand 1 in1 in2 out))
  (wire-on-change! in2 (gate-action 'nand 1 in1 in2 out)))

(define (gate-or out in1 in2)
  (wire-on-change! in1 (gate-action 'or 1 in1 in2 out))
  (wire-on-change! in2 (gate-action 'or 1 in1 in2 out)))

(define (gate-nor out in1 in2)
  (wire-on-change! in1 (gate-action 'nor 1 in1 in2 out))
  (wire-on-change! in2 (gate-action 'nor 1 in1 in2 out)))

(define (gate-xor out in1 in2)
  (wire-on-change! in1 (gate-action 'xor 2 in1 in2 out))
  (wire-on-change! in2 (gate-action 'xor 2 in1 in2 out)))

;=========================================
; przewody z bramek
;=========================================

(define (wire-not w)
  (define out (make-wire (wire-sim w)))
  (gate-not out w)
  out)

(define (wire-and w1 w2)
  (define out (make-wire (wire-sim w1)))
  (gate-and out w1 w2)
  out)

(define (wire-nand w1 w2)
  (define out (make-wire (wire-sim w1)))
  (gate-nand out w1 w2)
  out)

(define (wire-or w1 w2)
  (define out (make-wire (wire-sim w1)))
  (gate-or out w1 w2)
  out)

(define (wire-nor w1 w2)
  (define out (make-wire (wire-sim w1)))
  (gate-nor out w1 w2)
  out)

(define (wire-xor w1 w2)
  (define out (make-wire (wire-sim w1)))
  (gate-xor out w1 w2)
  out)

(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))