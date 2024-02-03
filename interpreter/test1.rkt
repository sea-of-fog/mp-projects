#lang plait
(require "interpreter.rkt")
(module+ test
  (print-only-errors #t))

; test to check if variables and functions can have the same names

(module+ test
  (test (run `{define {[fun fact (n) = {ifz n then 1 else {n * {fact {{n - 1}}}}}]}
                for {let fact be 5 in {fact {fact}}}})
        120))
