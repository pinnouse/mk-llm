#lang racket
(provide (all-defined-out) (all-from-out "first-order-miniKanren/tools.rkt" racket/pretty))
(require "first-order-miniKanren/tools.rkt" racket/pretty)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

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
(explore step (query (q) (reverseo q '(3 2 1))))

