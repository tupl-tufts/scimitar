#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../vec.rkt")

(provide
  (struct-out ir-expr)
  ir-val?
  (struct-out ir-num)
  (struct-out ir-vec)
  (struct-out ir-unit)
  (struct-out ir-poly)
  (struct-out ir-var)
  (struct-out ir-add)
  ir-neg ir-sub
  (struct-out ir-scmul)
  (struct-out ir-var-mul)
  (struct-out ir-tuple)
  (struct-out ir-vec-ix)
  (struct-out ir-vec-sum)
  (struct-out ir-cast)
  (struct-out ir-poly-app)
  (struct-out ir-thunk)
  ir-stmt?
  (struct-out ir-forall)
  (struct-out ir-subject-to)
  (struct-out ir-constraint)
  ir-dec?
  (struct-out ir-poly-dec))

;; expr
(struct
  ir-expr ()
  #:transparent)

(struct
  ir-val ir-expr ()
  #:transparent)

(struct/contract
  ir-num ir-val
  ([num number?])
  #:transparent)

(struct/contract
  ir-vec ir-val
  ([vec vec?])
  #:transparent)

(struct
  ir-unit ir-val ()
  #:transparent)

(struct/contract
  ir-poly ir-val
  ([poly poly?])
  #:transparent)

(struct/contract
  ir-var ir-expr
  ([name symbol?])
  #:transparent)

(struct/contract
  ir-add ir-expr
  ([lhs ir-expr?]
   [rhs ir-expr?])
  #:transparent)

(define-syntax-rule (ir-neg e)
  (ir-scmul (ir-num -1) e))

(define-syntax-rule (ir-sub e1 e2)
  (ir-add e1 (ir-neg e2)))

(struct/contract
  ir-scmul ir-expr
  ([lhs ir-expr?]
   [rhs ir-expr?])
  #:transparent)

(struct/contract
  ir-var-mul ir-expr
  ([lhs ir-expr?]
   [rhs ir-expr?])
  #:transparent)

(struct/contract
  ir-tuple ir-expr
  ([es (*list/c ir-expr? ir-expr? ir-expr?)])
  #:transparent)

(struct/contract
  ir-vec-ix ir-expr
  ([vec ir-expr?]
   [ix ir-expr?])
  #:transparent)

(struct/contract
  ir-vec-sum ir-expr
  ([id symbol?] ;; id is a "compile-time" variable
   [start ir-expr?]
   [end ir-expr?]
   [body ir-expr?])
  #:transparent)

(struct/contract
  ir-cast ir-expr
  ([e ir-expr?]
   [ty ty?])
  #:transparent)

(struct/contract
  ir-poly-app ir-expr
  ([f ir-expr?]
   [xs ir-expr?])
  #:transparent)

(struct/contract
  ir-thunk ir-expr
  ([body (or/c number? symbol? null? pair?)]
   [args (listof symbol?)]
   [force procedure?])
  #:transparent)

;; stmts
(struct
  ir-stmt ()
  #:transparent)

(struct/contract
  ir-forall ir-stmt
  ([id symbol?] ;; id is a "compile-time" variable
   [start ir-expr?]
   [end ir-expr?]
   [body ir-stmt?])
  #:transparent)

(struct/contract
  ir-subject-to ir-stmt
  ([Gamma_l env?]
   [ss (listof ir-stmt?)])
  #:transparent)

(struct/contract
  ir-constraint ir-stmt
  ([lhs ir-expr?]
   [ineq (symbols '<= '= '>=)]
   [rhs ir-expr?])
  #:transparent)

;; dec
(struct
  ir-dec ()
  #:transparent)

(struct/contract
  ir-poly-dec ir-dec
  ([f symbol?]
   [Gamma_f env?]
   [Gamma_r env?]
   [Gamma_l env?]
   [ss (listof ir-stmt?)])
  #:transparent)
