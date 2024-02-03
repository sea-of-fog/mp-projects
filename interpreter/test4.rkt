#lang plait
(require "interpreter.rkt")
(module+ test
  (print-only-errors #t))

; check if functions only use valid argument and function names in body
; TODO : add let to this!

(module+ test
  (test (run `{define {{fun f {x y} = {x + y}}} for {f (10 10)}})
        20)
  (test/exn (run `{define {{fun f {x y} = {k (x y)}}} for {f (10 10)}})
            "using unbound function in body")
  (test/exn (run `{define {{fun f {x y} = {x + z}}} for {f (10 10)}})
            "using unbound variable in body")
  (test (run `{define {{fun f {x y} = {let z be 10 in {x + z}}}} for {f (10 10)}})
        20))
