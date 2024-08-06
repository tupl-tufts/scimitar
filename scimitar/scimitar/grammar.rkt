#lang racket

(require "../contract-utils.rkt")
(require "../poly.rkt")
(require "../val.rkt")
(require "../ty.rkt")
(require "../util.rkt")

(provide
  (struct-out scimitar-expr)
  (struct-out scimitar-solve)
  (struct-out scimitar-typed)
  (struct-out scimitar-var)
  (struct-out scimitar-val)
  (struct-out scimitar-poly)
  (struct-out scimitar-tuple)
  (struct-out scimitar-switch)
  (struct-out scimitar-app)
  (struct-out scimitar-lambda)
  (struct-out scimitar-fix)
  ;; Sugar terms
  (struct-out scimitar-let)
  (struct-out scimitar-let*)
  (struct-out scimitar-begin)
  ;; Sugar for optimization language
  (struct-out scimitar-num)
  (struct-out scimitar-plus)
  (struct-out scimitar-minus)
  (struct-out scimitar-mul)
  (struct-out scimitar-lt)
  (struct-out scimitar-eq)
  (struct-out scimitar-gt)
  (struct-out scimitar-ref)
  (struct-out scimitar-if)
  (struct-out scimitar-for)
  (struct-out scimitar-sum)
  (struct-out scimitar-constraint)
  (struct-out scimitar-symbolic)
  (struct-out scimitar-infeasible))

(struct
  scimitar-expr ()
  #:transparent)

(struct/contract
  scimitar-solve scimitar-expr
  ([dir (or/c 'minimize 'maximize)]
   [res sym?]
   [obj scimitar-expr?]
   [locals (listof symbol?)]
   [problems (non-empty-listof scimitar-expr?)])
  #:transparent)

(struct/contract
  scimitar-typed scimitar-expr
  ([e scimitar-expr?]
   [ty ty?])
  #:transparent)

(struct/contract
  scimitar-var scimitar-expr
  ([x symbol?])
  #:transparent)

(struct/contract
  scimitar-val scimitar-expr
  ([v val?])
  #:transparent)

(struct/contract
  scimitar-poly scimitar-expr
  ([poly poly?])
  #:transparent)

(struct/contract
  scimitar-tuple scimitar-expr
  ([es (*list/c scimitar-expr? scimitar-expr? scimitar-expr?)])
  #:transparent)

(struct/contract
  scimitar-switch scimitar-expr
  ([op (-> ty? poly?)]
   [e scimitar-expr?]
   [es (listof scimitar-expr?)])
  #:transparent)

(struct/contract
  scimitar-app scimitar-expr
  ([f scimitar-expr?]
   [args scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-lambda scimitar-expr
  ([ps sym?]
   [body scimitar-expr?])
  #:transparent)

(define fun-entry?
  (list/c symbol? scimitar-expr?))

(struct/contract
  scimitar-fix scimitar-expr
  ([fs (listof fun-entry?)]
   [e scimitar-expr?])
  #:transparent)

;; Sugar terms
(struct/contract
  scimitar-let scimitar-expr
  ([bs (listof (list/c symbol? scimitar-expr?))]
   [e scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-let* scimitar-expr
  ([bs (listof (list/c symbol? scimitar-expr?))]
   [e scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-begin scimitar-expr
  ([es (listof scimitar-expr?)])
  #:transparent)

(struct/contract
  scimitar-num scimitar-expr
  ([n number?])
  #:transparent)

(struct/contract
  scimitar-plus scimitar-expr
  ([e1 scimitar-expr?]
   [e2 scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-minus scimitar-expr
  ([e1 scimitar-expr?]
   [e2 scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-mul scimitar-expr
  ([e1 scimitar-expr?]
   [e2 scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-lt scimitar-expr
  ([e1 scimitar-expr?]
   [e2 scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-eq scimitar-expr
  ([e1 scimitar-expr?]
   [e2 scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-gt scimitar-expr
  ([e1 scimitar-expr?]
   [e2 scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-ref scimitar-expr
  ([vec scimitar-expr?]
   [ix scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-if scimitar-expr
  ([eg scimitar-expr?]
   [et scimitar-expr?]
   [ef scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-for scimitar-expr
  ([i symbol?]
   [en scimitar-expr?]
   [em scimitar-expr?]
   [eb scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-sum scimitar-expr
  ([i symbol?]
   [en scimitar-expr?]
   [em scimitar-expr?]
   [eb scimitar-expr?])
  #:transparent)

(struct/contract
  scimitar-constraint scimitar-expr
  ([lhs scimitar-expr?]
   [ineq (symbols '<= '= '>=)]
   [rhs scimitar-expr?])
  #:transparent)

(struct
  scimitar-symbolic scimitar-expr
  ()
  #:transparent)

(struct
  scimitar-infeasible scimitar-expr
  ()
  #:transparent)
