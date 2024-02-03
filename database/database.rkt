#lang racket
(require rackunit)
; UWAGA! TESTY ZACZYNAJĄ SIĘ OD LINII ~250

(provide (struct-out column-info)
         (struct-out table)
         (struct-out and-f)
         (struct-out or-f)
         (struct-out not-f)
         (struct-out eq-f)
         (struct-out eq2-f)
         (struct-out lt-f)
         table-insert
         table-project
         table-sort
         table-select
         table-rename
         table-cross-join
         table-natural-join)

(define-struct column-info (name type) #:transparent)

(define-struct table (schema rows) #:transparent)

(define cities
  (table
   (list (column-info 'city    'string)
         (column-info 'country 'string)
         (column-info 'area    'number)
         (column-info 'capital 'boolean))
   (list (list "Wrocław" "Poland"  293 #f)
         (list "Warsaw"  "Poland"  517 #t)
         (list "Poznań"  "Poland"  262 #f)
         (list "Berlin"  "Germany" 892 #t)
         (list "Munich"  "Germany" 310 #f)
         (list "Paris"   "France"  105 #t)
         (list "Rennes"  "France"   50 #f))))

(define countries
  (table
   (list (column-info 'country 'string)
         (column-info 'population 'number))
   (list (list "Poland" 38)
         (list "Germany" 83)
         (list "France" 67)
         (list "Spain" 47))))

(define (empty-table columns) (table columns '()))

; insertion
(define (check-type col-info val) ; checks the input type
  (let ([type-symbol (column-info-type col-info)])
    (cond [(eq? type-symbol 'boolean) (boolean? val)]
          [(eq? type-symbol 'number) (number? val)]
          [(eq? type-symbol 'string) (string? val)]
          [(eq? type-symbol 'symbol) (symbol? val)])))

(define (table-insert row tab) 
  (if (= (length row) (length (table-schema tab)))
      (if (andmap check-type (table-schema tab) row)
          (table (table-schema tab) (cons row (table-rows tab)))
          (error "row doesn't fit schema"))
      (error "wrong row length")))

; extracts an element
; tail-recursive
(define (extract-col row schema col)
  (cond [(null? schema) (error "column not found")]
        [(eq? (column-info-name (car schema)) col) (car row)]
        [else (extract-col (cdr row) (cdr schema) col)]))

; returns a comparator depending on the input type
(define (typeless-lt x y)
  (cond [(number? x) (< x y)]
        [(string? x) (string<? x y)]
        [(symbol? x) (symbol<? x y)]
        [else (and (eq? x #f) (eq? y #t))]))

; calculates the column type from the schema
(define (extract-column-type schema col)
  (cond [(null? schema) (error "column not in schema")]
        [(eq? col (column-info-name (car schema))) (column-info-type (car schema))]
        [else (extract-column-type (cdr schema) col)]))

; projection
(define (table-project cols tab)
  (table (map
          (lambda (col)
            (column-info col (extract-column-type (table-schema tab) col)))
          cols)
         (map
          (lambda (row)
            (map (lambda (col) (extract-col row (table-schema tab) col)) cols))
          (table-rows tab))))

; generates a lexicographical comparator
(define (make-lt cols tab1 tab2)
  (lambda (row1 row2)
    (if (null? cols)
        #f
        (let ([val-1 (extract-col row1 (table-schema tab1) (car cols))]
              [val-2 (extract-col row2 (table-schema tab2) (car cols))])
          (cond [(typeless-lt val-1 val-2) #t]
                [(typeless-lt val-2 val-1) #f]
                [else ((make-lt (cdr cols) tab1 tab2) row1 row2)])))))


; SORTING
; lexicographical comparator
; rows passed as pairs of table rows
(define (lexi-lt row-pair-1 row-pair-2)
  (define (lt-it row1 row2)
    (cond [(null? row1) #f]
          [(typeless-lt (car row1) (car row2)) #t]
          [(typeless-lt (car row2) (car row1)) #f]
          [else (lt-it (cdr row1) (cdr row2))]))
    (lt-it (car row-pair-1) (car row-pair-2)))

(define (append-cols cols tab)
  (map cons
       (table-rows (table-project cols tab))
       (table-rows tab)))

(define (table-sort cols tab)
  (table (table-schema tab)
         (map cdr (sort (append-cols cols tab)
                        lexi-lt))))

; selection
(define-struct and-f (l r))
(define-struct or-f (l r))
(define-struct not-f (e))
(define-struct eq-f (name val))
(define-struct eq2-f (name name2))
(define-struct lt-f (name val))

(define (evaluate-form f schema row) ; wylicza rekurencyjnie wartość logiczną formuły na wierszu
    (cond [(and-f? f) (and (evaluate-form (and-f-l f) schema row)
                           (evaluate-form (and-f-r f) schema row))]
          [(or-f? f) (or (evaluate-form (or-f-l f) schema row)
                         (evaluate-form (or-f-r f) schema row))]
          [(not-f? f) (not (evaluate-form (not-f-e f) schema row))]
          [(eq-f? f) (eq? (eq-f-val f) (extract-col row schema (eq-f-name f)))]
          [(eq2-f? f) (eq? (extract-col row schema (eq2-f-name f))
                           (extract-col row schema (eq2-f-name2 f)))]
          [(lt-f? f) (typeless-lt
                                (extract-col row schema (lt-f-name f)) (lt-f-val f))]))

(define (table-select form tab)
  (table
   (table-schema tab)
   (filter (curry evaluate-form form (table-schema tab))
           (table-rows tab))))

; Zmiana nazwy
(define (table-rename col ncol tab)
  (table (map (lambda (colinfo)
                (if (eq? (column-info-name colinfo) col)
                    (column-info ncol (column-info-type colinfo))
                    colinfo))
              (table-schema tab))
         (table-rows tab)))


(define (cross-join-lists xs ys)
  (foldl append null
         (map (lambda ; the outer map generates the list of lists of concatenations as before
                  (row1)
                (map ; the inner map generates the list of concatenations of given row1 with all row2
                 (lambda (row2) (append row1 row2))
                 ys))
              xs)))

(define (natural-join-rows row1 row2 schema1 schema2)
  (append row1
          (map car (filter (lambda (x) (not (member (cdr x) schema1)))
                           (map cons row2 schema2)))))

; Cartesian join
(define (table-cross-join tab1 tab2)
  (table
   (append (table-schema tab1) (table-schema tab2))
   (cross-join-lists (table-rows tab1) (table-rows tab2))))

; Natural join
; Checks for column equality
(define (cols-eq? cols row1 row2 schema)
  (cond [(null? cols) #t]
        [(equal? (extract-col row1 schema (car cols))
                 (extract-col row2 schema (car cols)))
         (cols-eq? (cdr cols) row1 row2 schema)]
        [else #f]))

(define (natural-join-lists xs ys schema1 schema2)
  (let ([second-xs (map cdr xs)]
        [second-ys (map cdr ys)])
    (map (lambda (x) (natural-join-rows (car x) (cdr x) schema1 schema2))
         (foldl append null
                (map (lambda ; the outer map generates the list of lists of concatenations as before
                         (row1)
                       (map ; the inner map generates the list of concatenations of given row1 with all row2
                        (lambda (row2) (cons row1 row2))
                        second-ys))
                     second-xs)))))

(define (join-schema tab1 tab2)
  (append (table-schema tab1)
          (filter (lambda (x)
                    (not (member x (table-schema tab1))))
                  (table-schema tab2))))

(define (common-cols tab1 tab2)
  (map column-info-name
       (filter
        (lambda (x) (member x (table-schema tab2)))
        (table-schema tab1))))

(define (generate-segment acc rows schema cols)
  (cond [(null? rows) (cons acc null)]
        [(null? acc) (generate-segment (list (car rows)) (cdr rows) schema cols)]
        [(cols-eq? cols (car acc) (car rows) schema) (generate-segment (append acc (list (car rows)))
                                                                       (cdr rows)
                                                                       schema
                                                                       cols)]
        [else (cons acc rows)]))

(define (match-segment acc seg rows schema cols)
      (cond [(null? rows) (cons acc null)]
            [(lexi-lt (car rows) (car seg)) (match-segment acc seg (cdr rows) schema cols)]
            [(lexi-lt (car seg) (car rows)) (cons acc rows)]
            [else (generate-segment null rows schema cols)]))

(define (table-natural-join tab1 tab2)
  (let* ([cols (common-cols tab1 tab2)]
         [schema1 (table-schema tab1)]
         [schema2 (table-schema tab2)])
    (define (generate-rows acc rows1 rows2)
      (if (null? rows1)
          acc
          (let* ([seg1 (generate-segment null rows1 schema1 cols)]
                 [seg2 (match-segment null (car seg1) rows2 schema2 cols)])
            (generate-rows (append acc (natural-join-lists (car seg1) (car seg2) schema1 schema2))
                           (cdr seg1)
                           (cdr seg2)))))
    (table (join-schema tab1 tab2)
           (generate-rows null (append-cols cols (table-sort cols tab1))
                          (append-cols cols (table-sort cols tab2))))))

;=========================================================
; TESTS
;=========================================================

(check-equal? (table-insert '("Gliwice" "Poland" 100 #f) cities)
              (table
               (list (column-info 'city    'string)
                     (column-info 'country 'string)
                     (column-info 'area    'number)
                     (column-info 'capital 'boolean))
               (list (list "Gliwice" "Poland" 100 #f)
                     (list "Wrocław" "Poland"  293 #f)
                     (list "Warsaw"  "Poland"  517 #t)
                     (list "Poznań"  "Poland"  262 #f)
                     (list "Berlin"  "Germany" 892 #t)
                     (list "Munich"  "Germany" 310 #f)
                     (list "Paris"   "France"  105 #t)
                     (list "Rennes"  "France"   50 #f))))

(check-equal? (extract-column-type (table-schema cities) 'capital) 'boolean)

(check-equal? (extract-col (car (table-rows cities)) (table-schema cities) 'capital) #f)
(check-equal? (extract-col (car (table-rows cities)) (table-schema cities) 'country) "Poland")
(check-equal? (extract-col (car (table-rows cities)) (table-schema cities) 'area) 293)
(check-equal? (extract-col (car (table-rows cities)) (table-schema cities) 'city) "Wrocław")

(check-equal? (table-select (and-f (eq-f 'country "Poland")
                                   (lt-f 'area 500))
                            cities)
              (table (table-schema cities)
                     (list '("Wrocław" "Poland" 293 #f)
                           '("Poznań" "Poland" 262 #f))))

(check-equal? (table-select (not (eq2-f 'city 'country)) cities) cities)


(check-equal? (table-natural-join cities countries)
              (table
               (list
                (column-info 'city 'string)
                (column-info 'country 'string)
                (column-info 'area 'number)
                (column-info 'capital 'boolean)
                (column-info 'population 'number))
               '(("Rennes" "France" 50 #f 67)
                 ("Paris" "France" 105 #t 67)
                 ("Munich" "Germany" 310 #f 83)
                 ("Berlin" "Germany" 892 #t 83)
                 ("Poznań" "Poland" 262 #f 38)
                 ("Warsaw" "Poland" 517 #t 38)
                 ("Wrocław" "Poland" 293 #f 38))))

(check-equal? (table-sort '(city country area capital) (table-natural-join cities cities))
              (table-sort '(city country area capital) cities))
