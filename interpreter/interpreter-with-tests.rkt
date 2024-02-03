#lang plait

;======================
; ABSTRACT SYNTAX
;======================

(define-type-alias Value Number)

(define-type Op
  (add)
  (sub)
  (mul)
  (leq))

(define-type Exp
  (numE [n : Number])
  (varE [x : Symbol])
  (opE  [op : Op] [e1 : Exp] [e2 : Exp])
  (ifE  [ifCond : Exp] [ifTrue : Exp] [ifFalse : Exp])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (appE [fun : Symbol] [args : (Listof Exp)]))

(define-type FunDef
  (fun-def [name : Symbol]
          [args : (Listof Symbol)]
          [body : Exp]
          [arity : Number]))

(define-type Program
  (Prog [funcs : (Listof FunDef)] [body : Exp]))


;===================
; PARSER
;===================

(define (parse-Program [p : S-Exp]) : Program
  (if (s-exp-match? `{define {ANY ...} for ANY} p)
      (let ([ps (s-exp->list p)])
        (Prog (map parse-Func (s-exp->list (second ps)))
              (parse-Exp (fourth ps))))
      (error 'parse-Program "not a valid program definition")))

(define (parse-Op [s : Symbol]) : Op
  (cond [(eq? s '+)  (add)]
        [(eq? s '-)  (sub)]
        [(eq? s '*)  (mul)]
        [(eq? s '<=) (leq)]
        [else (error 'parse-Op "invalid operator")]))

(define (parse-Func [s : S-Exp]) : FunDef
  (if (s-exp-match? `{fun SYMBOL (SYMBOL ...) = ANY} s)
      (let ([ss (s-exp->list s)])
        (fun-def (s-exp->symbol (second ss))
                (map s-exp->symbol (s-exp->list (third ss)))
                (parse-Exp (fifth ss))
                (length (s-exp->list (third ss)))))
      (error 'parse-Func "invalid function definition")))

(define (parse-Exp [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{ifz ANY then ANY else ANY} s)
     (let ([ss (s-exp->list s)])
       (ifE (parse-Exp  (second ss))
            (parse-Exp  (fourth ss))
            (parse-Exp  (sixth ss))))]
    [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
     (let ([ss (s-exp->list s)])
       (letE (s-exp->symbol (second ss))
             (parse-Exp (fourth ss))
             (parse-Exp (sixth ss))))]
    [(s-exp-match? `{ANY SYMBOL ANY} s)
     (let ([ss (s-exp->list s)])
       (opE (parse-Op  (s-exp->symbol (second ss)))
            (parse-Exp (first ss))
            (parse-Exp (third ss))))]
    [(s-exp-match? `{SYMBOL (ANY ...)} s)
     (let ([ss (s-exp->list s)])
       (appE (s-exp->symbol (first ss))
             (map parse-Exp (s-exp->list (second ss)))))]
    [else (error 'parse-Exp "invalid syntax: didn't match")]))

;===================================
; SYNTAX CHECK (FOR DUPLICATE NAMES)
;===================================

(define (check-body [e : Exp] [var-names : (Listof Symbol)] [fun-names : (Listof Symbol)]) : Exp
  (type-case Exp e
    [(numE n) (numE n)]
    [(varE x) (if (member x var-names)
                  (varE x)
                  (error 'check-body "using unbound variable in body"))]
    [(opE op e1 e2) (opE op
                         (check-body e1 var-names fun-names)
                         (check-body e2 var-names fun-names))]
    [(ifE b l r) (ifE (check-body b var-names fun-names)
                      (check-body l var-names fun-names)
                      (check-body r var-names fun-names))]
    [(letE x e1 e2) (letE x
                          (check-body e1 var-names fun-names)
                          (check-body e2 (cons x var-names) fun-names))]
    [(appE f args) (if (member f fun-names)
                       (appE  f
                              (map (lambda (x) (check-body x var-names fun-names))
                                   args))
                       (error 'check-body "using unbound function in body"))]))
  
(define (check-fun-def [f : FunDef] [fun-names : (Listof Symbol)]) : FunDef
  (type-case FunDef f
    [(fun-def name args body arity) 
     (if (check-pairwise-different args)
         (fun-def name
                  args
                  (check-body body args fun-names)
                  arity)
         (error 'parse "function arguments have the same name"))]))


(define (check-Program [p : Program]) : Program
  (let ([fun-names (map fun-def-name (Prog-funcs p))])
    (if (check-pairwise-different fun-names)
        (Prog (map (lambda (x) (check-fun-def x fun-names)) (Prog-funcs p))
              (Prog-body p))
        (error 'parse "function names not different"))))

;==========================
; ENVIRONMENTS
;==========================

(define mt-env empty)
(define-type-alias (Bind 'a) (Symbol * 'a))
(define-type-alias (Env 'a) (Listof (Bind 'a)))

(define (lookup [x : Symbol] [env : (Env 'a)])
  (type-case (Listof (Bind 'a)) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst) (if (eq? x (fst b))
                      (snd b)
                      (lookup x rst))]))

(define (extend-env [x : Symbol] [v : 'a] [env : (Env 'a)])
  (cons (pair x v) env))

(define (extend-env-list [xs : (Listof Symbol)] [vs : (Listof 'a)] [env : (Env 'a)]) : (Env 'a)
  (type-case (Listof Symbol) xs
    [empty env]
    [(cons x xs) (type-case (Listof 'a) vs
                   [(cons v vs) (extend-env-list xs vs (extend-env x v env))]
                   [empty (error 'extend-env-list (string-append "no value for variable "
                                                                 (symbol->string x)))])]))

;==============================
; EVALUATOR
;==============================

(define (eval-Op [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) +]
    [(sub) -]
    [(mul) *]
    [(leq) (lambda (x y) (if (<= x y) 0 42))]))

(define (eval-Exp [e : Exp]
                  [env : (Env Value)]
                  [fun-env : (Env FunDef)]) : Value
  (type-case Exp e
    [(numE n)
     n]
    [(varE x)
     (lookup x env)]
    [(opE op e1 e2)
     ((eval-Op op)
      (eval-Exp e1 env fun-env)
      (eval-Exp e2 env fun-env))]
    [(ifE b l r)
     (if (= 0 (eval-Exp b env fun-env))
         (eval-Exp l env fun-env)
         (eval-Exp r env fun-env))]
    [(letE x e1 e2)
     (eval-Exp e2
           (extend-env x (eval-Exp e1 env fun-env) env)
           fun-env)]
    [(appE f args)
     (apply (lookup f fun-env)
            (map (lambda (e) (eval-Exp e env fun-env)) args)
            env
            fun-env)]))

(define (apply [f : FunDef] [args : (Listof Value)] env fun-env) : Value
  (type-case FunDef f
    [(fun-def name vars body arity)
     (if (= arity (length args))
         (eval-Exp body (extend-env-list vars args env) fun-env)
         (error 'apply "arity mismatch"))]))

(define (eval-Program [p : Program]) : Value
  (eval-Exp (Prog-body p)
            mt-env
            (extend-env-list (map fun-def-name (Prog-funcs p)) (Prog-funcs p) mt-env)))

;=======================
; FUNKCJE POMOCNICZE
;======================

(define (fifth [xs : (Listof 'a)]) : 'a
  (first (rest (rest (rest (rest xs))))))

(define (sixth [xs : (Listof 'a)]) : 'a
  (first (rest (rest (rest (rest (rest xs)))))))

(define (check-pairwise-different [ids : (Listof Symbol)]) : Boolean
  (type-case (Listof Symbol) ids
    (empty #t)
    ((cons id ids) (if (member id ids)
                       #f
                       (check-pairwise-different ids)))))

;=============================
; MAIN PIPELINE
;=============================

(define (run [s : S-Exp]) : Value
  (eval-Program (check-Program (parse-Program s))))