#lang racket
(provide (all-defined-out) (all-from-out "first-order-miniKanren/tools.rkt" racket/pretty))
(require "first-order-miniKanren/tools.rkt" racket/pretty)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define-syntax exp-single
  (syntax-rules ()
    ((_ q) (step-reset! (explore-take step q)))))
(define-syntax explore-count
  (syntax-rules ()
    ((_ query ...) (begin
                     (exp-single query) ...))))

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

;; TODO: Readline for the query
;; (explore step (query (q) (appendo q '(2 1) '(3 2 1))))
;; (explore step (query (q) (appendo q q '(3 2 1 3 2 1))))
(explore-count
 (query (q) (appendo q '(4 5) '(1 2 3 4 5)))
 (query (q) (appendo q q '(1 2 3 1 2 3)))
 (query (q) (appendo '(1 2 3 4 5) q '(1 2 3 4 5)))
 (query (q) (appendo '(1 2 3 4) '(5) q))
 (query (q r) (=/= q '()) (=/= q '(1)) (=/= q '(1 2)) (=/= q r) (appendo q r '(1 2 3 1 2 3)))
 (query (q r) (appendo q r '(1 2 3 4 5 6 7 8 9)) (appendo r q '(7 8 9 1 2 3 4 5 6))))
(println "Done. Goodbye.>")

