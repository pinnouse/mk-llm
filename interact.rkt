#lang racket
(provide (all-defined-out) (all-from-out "first-order-miniKanren/tools.rkt" racket/pretty))
(require "first-order-miniKanren/tools.rkt" racket/pretty)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

#| (define-syntax exp-single |#
#|   (syntax-rules () |#
#|     ((_ q) (step-reset! (explore-take step q))))) |#
#| (define-syntax explore-count |#
#|   (syntax-rules () |#
#|     ((_ query ...) (begin |#
#|                      (exp-single query) ...)))) |#

(define-syntax exp-single
  (syntax-rules ()
    ((_ q) (begin
             #| (newline) |#
             #| (displayln "Solving for query:") |#
             #| (pretty-print 'q) |#
             #| (newline) |#
             #| (step-reset-and-print! (drive/json step q)))))) |#
             (step-reset-and-print! (drive/depth-stdio step q))))))
(define-syntax explore-count
  (syntax-rules ()
    ((_ (query body ...) ...) (begin
                     (exp-single (query body ...)) ...))))

(define-relation (appendo ab c abc)
  (conde
   ((== '() ab) (== c abc))
   ((fresh (a b bc)
      (== `(,a . ,b) ab)
      (== `(,a . ,bc) abc)
      (appendo b c bc)))))

(define-relation (reverseo ys sy)
  (conde
   ((== '() ys) (== '() sy))
   ((fresh (first rest prefix)
      (== `(,first . ,rest) ys)
      ;; With a typical search strategy, there is no refutationally complete
      ;; ordering of the following two goals.  This ordering works well when
      ;; running in the forward direction, but not in the backward direction.
      (reverseo rest prefix)
      (appendo prefix `(,first) sy)))))

;; This is an interpreter for a simple Lisp.  Variables in this language are
;; represented namelessly, using De Bruijn indices.
;; Because it is implemented as a relation, we can run this interpreter with
;; unknowns in any argument position.  If we place unknowns in the `expr`
;; position, we can synthesize programs.
(define-relation (eval-expo expr env value)
  (conde ;; NOTE: this clause order is optimized for quine generation.
    ((fresh (body)
       (== `(lambda ,body) expr)      ;; expr is a procedure definition
       (== `(closure ,body ,env) value)))
    ;; If this is before lambda, quoted closures become likely.
    ((== `(quote ,value) expr))       ;; expr is a literal constant
    ((fresh (a*)
       (== `(list . ,a*) expr)        ;; expr is a list operation
       (eval-listo a* env value)))
    ((fresh (a d va vd)
       (== `(cons ,a ,d) expr)        ;; expr is a cons operation
       (== `(,va . ,vd) value)
       (eval-expo a env va)
       (eval-expo d env vd)))
    ((fresh (index)
       (== `(var ,index) expr)        ;; expr is a variable
       (lookupo index env value)))
    ((fresh (c va vd)
       (== `(car ,c) expr)            ;; expr is a car operation
       (== va value)
       (eval-expo c env `(,va . ,vd))))
    ((fresh (c va vd)
       (== `(cdr ,c) expr)            ;; expr is a cdr operation
       (== vd value)
       (eval-expo c env `(,va . ,vd))))
    ((fresh (rator rand arg env^ body)
       (== `(app ,rator ,rand) expr)  ;; expr is a procedure application
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env arg)
       (eval-expo body `(,arg . ,env^) value)))))

;; Lookup the value a variable is bound to.
;; Variables are represented namelessly using relative De Bruijn indices.
;; These indices are encoded as peano numerals: (), (s), (s s), etc.
(define-relation (lookupo index env value)
  (fresh (arg e*)
    (== `(,arg . ,e*) env)
    (conde
      ((== '() index) (== arg value))
      ((fresh (i* a d)
         (== `(s . ,i*) index)
         (== `(,a . ,d) e*)
         (lookupo i* e* value))))))

;; This helper evaluates arguments to a list construction.
(define-relation (eval-listo e* env value)
  (conde
    ((== '() e*) (== '() value))
    ((fresh (ea ed va vd)
       (== `(,ea . ,ed) e*)
       (== `(,va . ,vd) value)
       (eval-expo ea env va)
       (eval-listo ed env vd)))))

(define (evalo expr value) (eval-expo expr '() value))

#| (drive/depth-stdio |#
#|   step  |#
#|   (query (q) (evalo `(app (lambda . ,q) (quote (11 . 22))) (quote (22 . 11)))) |#
#|   ) |#

;; TODO: Readline for the query
;; (explore step (query (q) (appendo q '(2 1) '(3 2 1))))
;; (explore step (query (q) (appendo q q '(3 2 1 3 2 1))))
#| (explore-count |#
#|  (query (q) (appendo q '(4 5) '(1 2 3 4 5))) |#
#|  (query (q) (appendo q q '(1 2 3 1 2 3))) |#
#|  (query (q) (appendo '(1 2 3 4 5) q '(1 2 3 4 5))) |#
#|  (query (q) (appendo '(1 2 3 4) '(5) q)) |#
#|  (query (q r) (=/= q '()) (=/= q '(1)) (=/= q '(1 2)) (=/= q r) (appendo q r '(1 2 3 1 2 3))) |#
#|  (query (q r) (appendo q r '(1 2 3 4 5 6 7 8 9)) (appendo r q '(7 8 9 1 2 3 4 5 6)))) |#

(explore-count
  
  ; sanity-check
  ; (query (q) (evalo `(cons (quote 1) ,q) '(1 2)))
  ; pair-swap
  ; (query (q) (evalo `(app (lambda . ,q) (quote (11 . 22))) (quote (22 . 11))))
  (query (q)
         (evalo `(app (lambda . ,q) (quote (11 . 22))) (quote (22 . 11)))
         (evalo `(app (lambda . ,q) (quote (33 . 44))) (quote (44 . 33))))
  ; repeat-N
  ; (query (q) (evalo `(app (lambda . ,q) (quote (1 () a b x))) (quote ((1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x)))))
  ; drop-last-N
  ; (query (q) (evalo `(app (lambda . ,q) (quote ((1 #f a b x 0 1) (#f a b x 0 1 1) (a b x 0 1 1 #f) (b x 0 1 1 #f a) (x 0 1 1 #f a b)))) (quote ((1 #f a b x 0) (#f a b x 0 1) (a b x 0 1 1) (b x 0 1 1 #f) (x 0 1 1 #f a)))))
  ; bring-last-to-front-N
  ; (query (q) (evalo `(app (lambda . ,q) (quote ((1 b #f a b #f 0 a 1 x) (x #f a b #f 0 a 1 b 1) (a b #f 0 a x 1 b 1 #f) (b #f 0 a 1 x b 1 #f a) (0 a 1 b 1 #f x a b #f)))) (quote ((x 1 b #f a b #f 0 a 1) (1 x #f a b #f 0 a 1 b) (#f a b #f 0 a x 1 b 1) (a b #f 0 a 1 x b 1 #f) (#f 0 a 1 b 1 #f x a b)))))
  ; quine
  ; (query (q) (evalo q q))
  ; (query (q) (evalo `(app (lambda . ,q) (quote ((1 2 3) (4 5 6) (7 8 9)))) (quote ((1 4 7) (2 5 8) (3 6 9)))))
  (query (q)
    (evalo `(app (lambda . ,q) (quote (1 () a b x))) (quote ((1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x))))
    (evalo `(app (lambda . ,q) (quote (2 (3) 4 5 6))) (quote ((2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6)))))
  #| (query (q) |#
  #|   (evalo `(app (lambda . ,q) (quote (1 () a b x))) (quote ((1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x)))) |#
  #|   (evalo `(app (lambda . ,q) (quote (2 (3) 4 5 6))) (quote ((2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6))))) |#
  ; drop-last-N
  #| ((q) (evalo `(app (lambda . ,q) (quote ((1 #f a b x 0 1) (#f a b x 0 1 1) (a b x 0 1 1 #f) (b x 0 1 1 #f a) (x 0 1 1 #f a b)))) (quote ((1 #f a b x 0) (#f a b x 0 1) (a b x 0 1 1) (b x 0 1 1 #f) (x 0 1 1 #f a))))) |#
  #| (query (q) |#
  #|   (evalo `(app (lambda . ,q) (quote ((1 #f a b x 0 1) (#f a b x 0 1 1) (a b x 0 1 1 #f) (b x 0 1 1 #f a) (x 0 1 1 #f a b)))) (quote ((1 #f a b x 0) (#f a b x 0 1) (a b x 0 1 1) (b x 0 1 1 #f) (x 0 1 1 #f a)))) |#
  #|   (evalo `(app (lambda . ,q) (quote ((2 #t 3 4 5 6 7) (#t 3 4 5 6 7 2) (3 4 5 6 7 2 #t) (4 5 6 7 2 #t 3) (5 6 7 2 #t 3 4)))) (quote ((2 #t 3 4 5 6) (#t 3 4 5 6 7) (3 4 5 6 7 2) (4 5 6 7 2 #t) (5 6 7 2 #t 3))))) |#
  ; bring-last-to-front-N
  #| ((q) (evalo `(app (lambda . ,q) (quote ((1 b #f a b #f 0 a 1 x) (x #f a b #f 0 a 1 b 1) (a b #f 0 a x 1 b 1 #f) (b #f 0 a 1 x b 1 #f a) (0 a 1 b 1 #f x a b #f)))) (quote ((x 1 b #f a b #f 0 a 1) (1 x #f a b #f 0 a 1 b) (#f a b #f 0 a x 1 b 1) (a b #f 0 a 1 x b 1 #f) (#f 0 a 1 b 1 #f x a b))))) |#
  ; transpose
  #| (query (q) |#
  #|   (evalo `(app (lambda . ,q) (quote ((1 2 3) (4 5 6) (7 8 9)))) (quote ((1 4 7) (2 5 8) (3 6 9)))) |#
  #|   (evalo `(app (lambda . ,q) (quote ((a b c) (d e f) (g h i))))) (quote ((a d g) (b e h) (c f i)))) |#
  ; quine
  #| (query (q) (evalo q q)) |#
)
(printf "Done. Goodbye.>\n")

