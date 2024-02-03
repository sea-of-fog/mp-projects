;======================
; TESTING
;======================

; test to check if variables and functions can have the same names

(module+ test
  (test (run `{define {[fun fact (n) = {ifz n then 1 else {n * {fact {{n - 1}}}}}]}
                for {let fact be 5 in {fact {fact}}}})
        120))

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
; testing arity mismatches

(module+ test
  (test/exn (run `{define {{fun f {x y} = {x + y}}} for {f (3 4 5)}})
		"arity mismatch")
  (test/exn (run `{define {{fun f {} = 7}
                           {fun g {a b c} = {a + {b * c}}}} for {{g (1 2 3)} + {f (3)}}})
            "arity mismatch"))

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
            "invalid function definition"));======================
; TESTING
;======================

; test to check if variables and functions can have the same names

(module+ test
  (test (run `{define {[fun fact (n) = {ifz n then 1 else {n * {fact {{n - 1}}}}}]}
                for {let fact be 5 in {fact {fact}}}})
        120))

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
; testing arity mismatches

(module+ test
  (test/exn (run `{define {{fun f {x y} = {x + y}}} for {f (3 4 5)}})
		"arity mismatch")
  (test/exn (run `{define {{fun f {} = 7}
                           {fun g {a b c} = {a + {b * c}}}} for {{g (1 2 3)} + {f (3)}}})
            "arity mismatch"))

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
            "invalid function definition"));======================
; TESTING
;======================

; test to check if variables and functions can have the same names

(module+ test
  (test (run `{define {[fun fact (n) = {ifz n then 1 else {n * {fact {{n - 1}}}}}]}
                for {let fact be 5 in {fact {fact}}}})
        120))

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

; testing arity mismatches

(module+ test
  (test/exn (run `{define {{fun f {x y} = {x + y}}} for {f (3 4 5)}})
		"arity mismatch")
  (test/exn (run `{define {{fun f {} = 7}
                           {fun g {a b c} = {a + {b * c}}}} for {{g (1 2 3)} + {f (3)}}})
            "arity mismatch"))

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
