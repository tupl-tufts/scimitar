#lang racket

(require (prefix-in clp- "clp-ffi.rkt"))
(require "if.rkt")
(require "util.rkt")

(provide clp)

(define (clp-readMps-2 model file)
  (clp-readMps model file #t #f))

(define (clp-writeMps-2 model file)
  (clp-writeMps model file 0 1 1))

(define (clp-addCol model name lb ub obj rows coefs)
  (let ((column (clp-getNumCols model)))
    (clp-addColumns model `(,lb) `(,ub) `(,obj) `(,rows) `(,coefs))
    (clp-setColumnName model column name)
    (when (not (equal? 'continuous (range-class lb ub)))
      (clp-setInteger model column))))

(define (clp-addRows-2 model ixs elems senses rhss)
  (let ((rowLower (map lower-bound senses rhss))
        (rowUpper (map upper-bound senses rhss)))
    (clp-addRows model rowLower rowUpper ixs elems)))

(define (clp-addRow model name cols coefs sense rhs)
  (let ((rowLower (lower-bound sense rhs))
        (rowUpper (upper-bound sense rhs))
        (row (clp-getNumRows model)))
    (clp-addRows model `(,rowLower) `(,rowUpper) `(,cols) `(,coefs))
    (clp-setRowName model row name)))

(define (clp-addSOS model rowStarts colIndices weights type)
  (error (format "CLP does not support SOS")))

(define (clp-setRowLower model i val)
  (let ((lb (clp-getRowLower model)))
    (clp-chgRowLower model (list-set lb i val))))

(define (clp-setRowUpper model i val)
  (let ((ub (clp-getRowUpper model)))
    (clp-chgRowUpper model (list-set ub i val))))

(define (clp-setColLower model i val)
  (let ((lb (clp-getColLower model)))
    (clp-chgColumnLower model (list-set lb i val))))

(define (clp-setColUpper model i val)
  (let ((ub (clp-getColUpper model)))
    (clp-chgColumnUpper model (list-set ub i val))))

(define (clp-isInteger model i)
  #f)

(define (clp-setContinuous model i)
  (void))

(define (clp-setInteger model i)
  (error (format "CLP does not support setting column ~v as integer" i)))

(define (clp-setColTypes model types)
  (define i 0)
  (for ([ty types])
    (if (equal? ty 'continuous)
      (clp-setContinuous model i)
      (clp-setInteger model i))
    (set! i (+ i 1))))

(define (clp-getColTypes model)
  (make-list (clp-getNumCols model) 'continuous))

(define (clp-setPreprocess m v)
  (if (eq? v 'off)
    (void)
    (error (format "CLP does not support setting preprocess to ~v" v))))

(define (clp-isProvenInfeasible model)
  (or (clp-isProvenPrimalInfeasible model)
      (clp-isProvenDualInfeasible model)))

(define (clp-loadProblem-2 model ixs vals collb colub obj senses rhss)
  (clp-loadProblem model ixs vals collb colub obj
                   (map lower-bound senses rhss)
                   (map upper-bound senses rhss)))

(define (clp-dumpProblem model)
  (let* ((vSs (clp-getVectorStarts model))
         (Is  (clp-getIndices model))
         (Es  (clp-getElements model))
         (cLs (clp-getColLower model))
         (cUs (clp-getColUpper model))
         (oCs (clp-getObjCoefficients model))
         (rLs (clp-getRowLower model))
         (rUs (clp-getRowUpper model))
         (rSs (map bound-constraint rLs rUs))
         (rRs (map bound-rhs rLs rUs)))
    (let-values ([(ixs elems) (group-elements vSs Is Es)])
      (values ixs elems cLs cUs oCs rSs rRs))))

(define clp
  (solver
    clp-version

    clp-Simplex?
    clp-newModel
    clp-deleteModel

    clp-loadProblem-2
    clp-dumpProblem
    clp-setColumnName
    clp-setRowName
    clp-readMps-2
    clp-writeMps-2
    clp-addRows-2
    clp-addColumns

    clp-addCol
    clp-addRow
    clp-addSOS

    clp-optimization_direction?
    clp-log_level?

    clp-getObjSense
    clp-setObjSense
    clp-getRowLower
    clp-setRowLower
    clp-getRowUpper
    clp-setRowUpper
    clp-getObjCoefficients
    clp-getColLower
    clp-setColLower
    clp-getColUpper
    clp-setColUpper
    clp-isInteger
    clp-setContinuous
    clp-setInteger
    clp-setColTypes
    clp-getColTypes
    clp-getNumElements
    clp-getVectorStarts
    clp-getIndices
    clp-getElements
    clp-getObjValue
    clp-printModel

    clp-setPreprocess
    clp-setLogLevel

    clp-status?

    clp-initialSolve

    clp-getNumRows
    clp-getNumCols

    clp-isAbandoned
    clp-isProvenOptimal
    clp-isProvenInfeasible

    clp-getRowActivity
    clp-getColSolution
    ))
