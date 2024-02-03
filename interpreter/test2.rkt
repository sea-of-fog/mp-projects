#lang plait
(require "interpreter.rkt")
(module+ test
  (print-only-errors #t))

; test to check if the intepreter checks for uniqueness of
; function ids and parameter ids

(module+ test
  (test/exn (run `{define {{fun f (x x) = {x + x}}} for 1})
            "function arguments have the same name")
  (test/exn (run `{define {{fun f (x) = x}
                           {fun f (x y) = (x + y)}} for 1})
            "function names not different")
  (test/exn (run `{define {{fun f (x) = x}
                           {fun f (x x) = (x + x)}} for 1})
            "function names not different"))
