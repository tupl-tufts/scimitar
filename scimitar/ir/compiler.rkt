#lang racket

(require "../contract-utils.rkt")
(require "../util.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "force.rkt")
(require "const-reduce.rkt")
(require "elim-st.rkt")
(require "expand-var-mul.rkt")
(require "expand-vec-ix.rkt")
(require "var-rename.rkt")
(require "grammar.rkt")
(require "linearize.rkt")
(require "lower.rkt")
(require "ty.rkt")

(provide
  ir-compile)

(define/contract (ir-compile pd Gamma rho phi)
  (-> ir-dec? env? const-env? poly-env? poly?)
  (let* (;; Check the types
         (Gamma (env-union-key
                  (env-map-vals (compose ir-val-check ir-num) rho ty?)
                  Gamma))
         (td (ir-dec-force-thunk pd))
         (_ (ir-dec-check td Gamma))
         ;; Reduce const exprs, flatten AST to constraints with nested subject-tos
         (rd (ir-dec-const-reduce td rho))
         ;; Rename vars to avoid conflicts
         (vd (ir-dec-var-rename rd))
         ;; Flatten nested subject-tos
         (ed (ir-dec-elim-st vd))
         ;; Transform every expression to a linear expression using fresh variables
         (ld (ir-dec-linearize ed Gamma))
         ;; Expand vector indexing over variables to indicator indexing
         (ev (ir-dec-expand-vec-ix ld))
         ;; Expand variable multiplication from non-scalar values to tuples of scalars
         (ed (ir-dec-expand-var-mul ev))
         ;; Compile the program into a poly
         (p (ir-dec-lower ed phi)))
    p))
