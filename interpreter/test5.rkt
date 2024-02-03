#lang plait
(require "interpreter.rkt")
(module+ test
  (print-only-errors #t))

; parsing tests

; operators

(module+ test
  (test (parse-Op '+)
        (add))
  (test (parse-Op '-)
        (sub))
  (test (parse-Op '*)
        (mul))
  (test (parse-Op '<=)
        (leq))
  (test/exn (parse-Op '/)
            "invalid operator")
  (test/exn (parse-Op '=)
            "invalid operator"))

; expressions
(module+ test
  (test (parse-Exp `{let x be 10 in {x + y}})
        (letE 'x (numE 10) (opE (add) (varE 'x) (varE 'y))))
  (test (parse-Exp `10)
        (numE 10))
  (test (parse-Exp `x)
        (varE 'x))
  (test (parse-Exp `y)
        (varE 'y))
  (test (parse-Exp `{ifz 27 then {y - 20} else {17 + x}})
        (ifE (numE 27)
             (opE (sub) (varE 'y) (numE 20))
             (opE (add) (numE 17) (varE 'x))))
  (test/exn (parse-Exp `{+ 1 2})
            "invalid syntax: didn't match")
  (test (parse-Exp `{+ (1 2)})
        (appE '+ (list (numE 1) (numE 2))))
  (test/exn (parse-Exp `{1 / 2})
            "invalid operator"))

; function definitions

(module+ test
  (test (parse-Func `{fun f {x} = 1})
        (fun-def 'f (list 'x) (numE 1) 1))
  (test (parse-Func `{fun alabama {} = {x + y}})
        (fun-def 'alabama empty (opE (add) (varE 'x) (varE 'y)) 0))
  (test (parse-Func `{fun alfa {x y} = {let z be 10 in {x + {y * z}}}})
        (fun-def 'alfa (list 'x 'y) (letE 'z (numE 10) (opE (add)
                                                            (varE 'x)
                                                            (opE (mul) (varE 'y) (varE 'z)))) 2))
  (test/exn (parse-Func `{fun jerzy {x} = {1 / x}})
            "invalid operator")
  (test/exn (parse-Func `{fun mombasa x = {1 + x}})
            "invalid function definition"))

; program definitions

(module+ test
  (test/exn (parse-Program `{define {fun f {x} = x} for 1})
            "invalid function definition"))
