#lang racket

(require scimitar/contract-utils)

(provide
  imp-expr?
  (struct-out imp-plus)
  (struct-out imp-minus)
  (struct-out imp-le)
  (struct-out imp-eq)
  (struct-out imp-bool)
  (struct-out imp-not)
  (struct-out imp-and)
  (struct-out imp-or)
  (struct-out imp-nat)
  (struct-out imp-var)
  imp-stmt?
  (struct-out imp-skip)
  (struct-out imp-assign)
  (struct-out imp-if)
  (struct-out imp-while))

(struct imp-expr ()
  #:transparent)

(struct/contract imp-plus imp-expr
  ([lhs imp-expr?]
   [rhs imp-expr?])
  #:transparent)
(struct/contract imp-minus imp-expr
  ([lhs imp-expr?]
   [rhs imp-expr?])
  #:transparent)
(struct/contract imp-le imp-expr
  ([lhs imp-expr?]
   [rhs imp-expr?])
  #:transparent)
(struct/contract imp-eq imp-expr
  ([lhs imp-expr?]
   [rhs imp-expr?])
  #:transparent)
(struct/contract imp-bool imp-expr
  ([val boolean?])
  #:transparent)
(struct/contract imp-not imp-expr
  ([expr imp-expr?])
  #:transparent)
(struct/contract imp-and imp-expr
  ([lhs imp-expr?]
   [rhs imp-expr?])
  #:transparent)
(struct/contract imp-or imp-expr
  ([lhs imp-expr?]
   [rhs imp-expr?])
  #:transparent)
(struct/contract imp-nat imp-expr
  ([val natural?])
  #:transparent)
(struct/contract imp-var imp-expr
  ([name symbol?])
  #:transparent)

(struct imp-stmt ()
  #:transparent)

(struct/contract imp-skip imp-stmt
  ()
  #:transparent)
(struct/contract imp-assign imp-stmt
  ([name symbol?]
   [expr imp-expr?])
  #:transparent)
(struct/contract imp-if imp-stmt
  ([pred imp-expr?]
   [true imp-stmt?]
   [false imp-stmt?])
  #:transparent)
(struct/contract imp-while imp-stmt
  ([pred imp-expr?]
   [body (listof imp-stmt?)])
  #:transparent)
