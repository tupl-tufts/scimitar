#lang racket

(require (prefix-in cbc- "cbc-ffi.rkt"))
(require "util.rkt")
(require "if.rkt")

(provide cbc)

(define (cbc-addRows model ixs elems senses rhss)
  (let ((number (maxlen elems senses rhss)))
    (for-each (curry cbc-addRow model "")
      (pad-to/null number '() ixs)
      (pad-to/null number '() elems)
      senses rhss)))

(define (cbc-addColumns model columnLower columnUpper objective ixs elems)
  (let ((number (maxlen columnLower columnUpper objective elems)))
    (for [(lb  (pad-to/null number 0.0 columnLower))
          (ub  (pad-to/null number 0.0 columnUpper))
          (obj (pad-to/null number 0.0 objective))
          (ix  (pad-to/null number '() ixs))
          (el  (pad-to/null number '() elems))]
      (cbc-addCol model "" lb ub obj ix el))))

(define (cbc-loadProblem-2 model ixs vals collb colub obj senses rhss)
  (cbc-loadProblem model ixs vals collb colub obj
                   (map lower-bound senses rhss)
                   (map upper-bound senses rhss)))

(define (cbc-dumpProblem model)
  (let* ((vSs (cbc-getVectorStarts model))
         (Is  (cbc-getIndices model))
         (Es  (cbc-getElements model))
         (cLs (cbc-getColLower model))
         (cUs (cbc-getColUpper model))
         (oCs (cbc-getObjCoefficients model))
         (rLs (cbc-getRowLower model))
         (rUs (cbc-getRowUpper model))
         (rSs (map bound-constraint rLs rUs))
         (rRs (map bound-rhs rLs rUs)))
    (let-values ([(ixs elems) (group-elements vSs Is Es)])
      (values ixs elems cLs cUs oCs rSs rRs))))

(define (cbc-setColTypes model types)
  (define i 0)
  (for ([ty types])
    (if (equal? ty 'continuous)
      (cbc-setContinuous model i)
      (cbc-setInteger model i))
    (set! i (+ i 1))))

(define (cbc-getColTypes model)
  (build-list (cbc-getNumCols model)
    (lambda (n) (if (cbc-isInteger model n) 'integer 'continuous))))

(define cbc
  (solver
    cbc-getVersion

    cbc-Model?
    cbc-newModel
    cbc-deleteModel

    cbc-loadProblem-2
    cbc-dumpProblem
    cbc-setColName
    cbc-setRowName
    cbc-readMps
    cbc-writeMps
    cbc-addRows
    cbc-addColumns

    cbc-addCol
    cbc-addRow
    cbc-addSOS

    cbc-optimization_direction?
    cbc-log_level?

    cbc-getObjSense
    cbc-setObjSense
    cbc-getRowLower
    cbc-setRowLower
    cbc-getRowUpper
    cbc-setRowUpper
    cbc-getObjCoefficients
    cbc-getColLower
    cbc-setColLower
    cbc-getColUpper
    cbc-setColUpper
    cbc-isInteger
    cbc-setContinuous
    cbc-setInteger
    cbc-setColTypes
    cbc-getColTypes
    cbc-getNumElements
    cbc-getVectorStarts
    cbc-getIndices
    cbc-getElements
    cbc-getObjValue
    cbc-printModel

    cbc-setPreprocess
    cbc-setLogLevel

    cbc-status?

    cbc-solve

    cbc-getNumRows
    cbc-getNumCols

    cbc-isAbandoned
    cbc-isProvenOptimal
    cbc-isProvenInfeasible

    cbc-getRowActivity
    cbc-getColSolution
    ))
