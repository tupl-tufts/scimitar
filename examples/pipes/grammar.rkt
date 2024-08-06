#lang racket

(require scimitar/contract-utils)

(provide
  plumbing?
  (struct-out junction)
  (struct-out source)
  (struct-out sink)
  (struct-out pipe))

(struct/contract
  plumbing ()
  #:transparent)

(struct/contract
  junction plumbing
  ()
  #:transparent)

(struct/contract
  source junction
  ([flow number?])
  #:transparent)

(struct/contract
  sink junction
  ()
  #:transparent)

(struct/contract
  pipe plumbing
  ([input symbol?]
   [output symbol?]
   [flow number?])
  #:transparent)
