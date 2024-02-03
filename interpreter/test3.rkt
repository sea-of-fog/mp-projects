#lang plait
(require "interpreter.rkt")
(module+ test
  (print-only-errors #t))

; testing arity mismatches

(module+ test
  (test/exn (run `{define {{fun f {x y} = {x + y}}} for {f (3 4 5)}})
		"arity mismatch")
  (test/exn (run `{define {{fun f {} = 7}
                           {fun g {a b c} = {a + {b * c}}}} for {{g (1 2 3)} + {f (3)}}})
            "arity mismatch"))
