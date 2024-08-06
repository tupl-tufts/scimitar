#lang racket

(require (for-syntax syntax/parse))
(require racket/runtime-path)
(require (only-in "../params.rkt" solver-laziness))
(require "if.rkt")

(provide
  current-solver
  solver-loader-solver
  clp cbc gurobi

  getVersion

  Model?
  newModel
  deleteModel

  loadProblem
  dumpProblem
  setColName
  setRowName
  readMps
  writeMps
  addRows
  addColumns

  addCol
  addRow
  addSOS

  optimization_direction?
  log_level?

  getObjSense
  setObjSense
  getRowLower
  setRowLower
  getRowUpper
  setRowUpper
  getObjCoefficients
  getColLower
  setColLower
  getColUpper
  setColUpper
  isInteger
  setContinuous
  setInteger
  setColTypes
  getColTypes
  getNumElements
  getVectorStarts
  getIndices
  getElements
  getObjValue
  printModel

  setPreprocess
  setLogLevel

  status?

  solve

  getNumRows
  getNumCols

  isAbandoned
  isProvenOptimal
  isProvenInfeasible

  getRowActivity
  getColSolution)

(struct solver-loader
  (solver)
  #:mutable)

(define-syntax (define-solver stx)
  (syntax-parse stx
    [(_ name)
     (with-syntax ([path (gensym)])
       #'(begin
           (define-runtime-path path
             (string-append (symbol->string 'name) ".rkt"))
           (define name
             (solver-loader
               (lambda ()
                 (dynamic-require path 'name))))))]))

(define current-solver
  (make-derived-parameter
    (make-parameter (void)
      (invariant-assertion
        (-> solver-loader? solver-loader?)
        (lambda (loader)
          (when (equal? (solver-laziness) 'set)
            (set-solver-loader-solver! loader
              ((solver-loader-solver loader))))
          loader)))
    identity
    (lambda (loader)
      (let ((s-or-l (solver-loader-solver loader)))
        (if (solver? s-or-l)
          s-or-l
          (let ((solver (s-or-l)))
            (set-solver-loader-solver! loader solver)
            solver))))))

(define-solver clp)
(define-solver cbc)
(define-solver gurobi)

(define (getVersion . vs)              (apply (solver-getVersion (current-solver)) vs))

(define (Model? . vs)                  (apply (solver-Model? (current-solver)) vs))
(define (newModel . vs)                (apply (solver-newModel (current-solver)) vs))
(define (deleteModel . vs)             (apply (solver-deleteModel (current-solver)) vs))

(define (loadProblem . vs)             (apply (solver-loadProblem (current-solver)) vs))
(define (dumpProblem . vs)             (apply (solver-dumpProblem (current-solver)) vs))
(define (setColName . vs)              (apply (solver-setColName (current-solver)) vs))
(define (setRowName . vs)              (apply (solver-setRowName (current-solver)) vs))
(define (readMps . vs)                 (apply (solver-readMps (current-solver)) vs))
(define (writeMps . vs)                (apply (solver-writeMps (current-solver)) vs))
(define (addRows . vs)                 (apply (solver-addRows (current-solver)) vs))
(define (addColumns . vs)              (apply (solver-addColumns (current-solver)) vs))

(define (addCol . vs)                  (apply (solver-addCol (current-solver)) vs))
(define (addRow . vs)                  (apply (solver-addRow (current-solver)) vs))
(define (addSOS . vs)                  (apply (solver-addSOS (current-solver)) vs))

(define (optimization_direction? . vs) (apply (solver-optimization_direction? (current-solver)) vs))
(define (log_level? . vs)              (apply (solver-log_level? (current-solver)) vs))

(define (getObjSense . vs)             (apply (solver-getObjSense (current-solver)) vs))
(define (setObjSense . vs)             (apply (solver-setObjSense (current-solver)) vs))
(define (getRowLower . vs)             (apply (solver-getRowLower (current-solver)) vs))
(define (setRowLower . vs)             (apply (solver-setRowLower (current-solver)) vs))
(define (getRowUpper . vs)             (apply (solver-getRowUpper (current-solver)) vs))
(define (setRowUpper . vs)             (apply (solver-setRowUpper (current-solver)) vs))
(define (getObjCoefficients . vs)      (apply (solver-getObjCoefficients (current-solver)) vs))
(define (getColLower . vs)             (apply (solver-getColLower (current-solver)) vs))
(define (setColLower . vs)             (apply (solver-setColLower (current-solver)) vs))
(define (getColUpper . vs)             (apply (solver-getColUpper (current-solver)) vs))
(define (setColUpper . vs)             (apply (solver-setColUpper (current-solver)) vs))
(define (isInteger . vs)               (apply (solver-isInteger (current-solver)) vs))
(define (setContinuous . vs)           (apply (solver-setContinuous (current-solver)) vs))
(define (setInteger . vs)              (apply (solver-setInteger (current-solver)) vs))
(define (setColTypes . vs)             (apply (solver-setColTypes (current-solver)) vs))
(define (getColTypes . vs)             (apply (solver-getColTypes (current-solver)) vs))
(define (getNumElements . vs)          (apply (solver-getNumElements (current-solver)) vs))
(define (getVectorStarts . vs)         (apply (solver-getVectorStarts (current-solver)) vs))
(define (getIndices . vs)              (apply (solver-getIndices (current-solver)) vs))
(define (getElements . vs)             (apply (solver-getElements (current-solver)) vs))
(define (getObjValue . vs)             (apply (solver-getObjValue (current-solver)) vs))
(define (printModel . vs)              (apply (solver-printModel (current-solver)) vs))

(define (setPreprocess . vs)           (apply (solver-setPreprocess (current-solver)) vs))
(define (setLogLevel . vs)             (apply (solver-setLogLevel (current-solver)) vs))

(define (status? . vs)                 (apply (solver-status? (current-solver)) vs))

(define (solve . vs)                   (apply (solver-solve (current-solver)) vs))

(define (getNumRows . vs)              (apply (solver-getNumRows (current-solver)) vs))
(define (getNumCols . vs)              (apply (solver-getNumCols (current-solver)) vs))

(define (isAbandoned . vs)             (apply (solver-isAbandoned (current-solver)) vs))
(define (isProvenOptimal . vs)         (apply (solver-isProvenOptimal (current-solver)) vs))
(define (isProvenInfeasible . vs)      (apply (solver-isProvenInfeasible (current-solver)) vs))

(define (getRowActivity . vs)          (apply (solver-getRowActivity (current-solver)) vs))
(define (getColSolution . vs)          (apply (solver-getColSolution (current-solver)) vs))

