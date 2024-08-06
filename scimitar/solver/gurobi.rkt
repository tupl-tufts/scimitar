#lang racket

(require (prefix-in gurobi- "gurobi-ffi.rkt"))
(require "util.rkt")
(require "if.rkt")

(provide gurobi current-gurobi-env getIIS)

(define current-gurobi-env
  (make-parameter (void)
    (invariant-assertion
      (-> gurobi-Env? gurobi-Env?)
      identity)))

(define (ensure-env)
  (when (void? (current-gurobi-env))
    (let ((env (gurobi-emptyenv)))
      ;(gurobi-setstrparam env 'WLSAccessID str)
      ;(gurobi-setstrparam env 'WLSSecret str)
      ;(gurobi-setintparam env 'LicenseID int)
      (gurobi-setintparam env 'OutputFlag 0)
      (gurobi-startenv    env)
      (current-gurobi-env env))))

(define (getVersion)
  (let-values ([(maj min tech) (gurobi-version)])
    (format "~a.~a.~a" maj min tech)))

(define (newModel)
  (ensure-env)
  (gurobi-newmodel (current-gurobi-env) "" '() '() '() #f '()))

(define (clearModel model)
  (gurobi-setobjective model 'minimize 0.0 '() '() '() '() '())
  (gurobi-delvars model (range (getNumCols model)))
  (gurobi-delconstrs model (range (getNumRows model)))
  (gurobi-updatemodel model))

(define (dumpProblem model)
  (let*-values ([(nr)  (getNumRows model)]
                [(rSs) (gurobi-getcharattrarray model 'Sense 0 nr)]
                [(rSs) (map gurobi-c->constraint_sense rSs)]
                [(rRs) (gurobi-getdblattrarray model 'RHS 0 nr)]
                [(nc)  (getNumCols model)]
                [(oCs) (gurobi-getdblattrarray model 'Obj 0 nc)]
                [(cLs) (gurobi-getdblattrarray model 'LB 0 nc)]
                [(cUs) (gurobi-getdblattrarray model 'UB 0 nc)]
                [(starts inds vals) (gurobi-getvars model 0 nc)]
                [(ixs elems) (group-elements starts inds vals)])
         (values ixs elems cLs cUs oCs rSs rRs)))

(define (copyModel m1 m2)
  (let* ((mN  (gurobi-getstrattr m1 'ModelName))
         ; objective
         (oS  (gurobi-getintattr m1 'ModelSense))
         (oC  (gurobi-getdblattr m1 'ObjCon))
         ; columns
         (nc  (getNumCols m1))
         (cNs (gurobi-getstrattrarray m1 'VarName 0 nc))
         (cTs (map gurobi-c->variable_type (gurobi-getcharattrarray m1 'VType 0 nc)))
         ; rows
         (nr  (getNumRows m1))
         (rNs (gurobi-getstrattrarray m1 'ConstrName 0 nr)))
    (let*-values ([(ixs elems cLs cUs oCs rSs rRs) (dumpProblem m1)])
      (gurobi-setstrattr m2 'ModelName mN)
      (gurobi-setintattr m2 'ModelSense oS)
      (gurobi-addconstrs m2 '() '() rSs rRs rNs)
      (gurobi-addvars m2 ixs elems oCs cLs cUs cTs cNs)
      (gurobi-setdblattr m2 'ObjCon oC))))

(define (loadProblem model ixs elems collb colub obj senses rhss)
  (clearModel model)
  (let ((lmodel (gurobi-loadmodel
                  (gurobi-getenv model)
                  "" 'minimize 0 obj
                  senses rhss ixs elems
                  collb colub
                  '() '() '())))
    (copyModel lmodel model)
    (gurobi-freemodel lmodel)))

(define (setColName model col name)
  (gurobi-setstrattrelement model 'VarName col name))

(define (setRowName model row name)
  (gurobi-setstrattrelement model 'ConstrName row name))

(define (basepath->filepath basepath)
  (let-values ([(_ f d) (split-path basepath)])
    (define (error-file-not-found . unused)
      (error (format "Unable to open mps input file ~a" basepath)))
    (when d
      (error-file-not-found))
    (define (test p)
      (if (file-exists? p)
        p
        (let ((p (string-append p ".gz")))
          (if (file-exists? p)
            p
            (error-file-not-found)))))
    (define (close-to base)
      (let*-values ([(bb fb db) (split-path base)]
                    [(bb) (if (equal? 'relative bb) (string->path "./") bb)]
                    [(fb) (path->string fb)])
        (let ((fs (with-handlers ([exn:fail? error-file-not-found])
                    (find-files
                      (lambda (p)
                        (or (equal? bb p)
                            (let*-values ([(bp fp dp) (split-path p)]
                                          [(fp) (path->string fp)])
                              (and (string-prefix? fp fb)
                                   (file-exists? p)))))
                      bb #:skip-filtered-directory? #t))))
          (cond
            [(null? fs) (error-file-not-found)]
            [(equal? fs `(,bb)) (error-file-not-found)]
            ; there may be more, but who cares
            [else (cadr fs)]))))
    (cond
      [(not (string-contains? (path->string f) "."))
       (test (string-append basepath ".mps"))]
      [(string-suffix? basepath ".mps") (test basepath)]
      [(string-suffix? basepath ".") (test basepath)]
      [else (close-to basepath)])))

(define (readMps model basename)
  (clearModel model)
  (let ((rmodel (gurobi-readmodel
                  (gurobi-getenv model)
                  (basepath->filepath basename))))
    (copyModel rmodel model)
    (gurobi-freemodel rmodel)))

(define (writeMps model filename)
  (let ((filename (string-append filename ".mps.gz")))
    (gurobi-write model filename)))

(define (addRows model ixs elems senses rhss)
  (gurobi-addconstrs model ixs elems senses rhss '()))

(define (addColumns model columnLower columnUpper objective ixs elems)
  (gurobi-addvars model ixs elems objective columnLower columnUpper
                  (map range-class columnLower columnUpper) '()))

(define (addCol model name lb ub obj rows coefs)
  (gurobi-addvar model rows coefs obj lb ub (range-class lb ub) name))

(define (addRow model name cols coefs sense rhs)
  (gurobi-addconstr model cols coefs sense rhs name))

(define (addSOS model rowStarts colIndices weights type)
  (error "Gurobi addSOS Not yet implemented"))

(define (getObjSense model)
  (gurobi-c->obj_sense (gurobi-getintattr model 'ModelSense)))

(define (setObjSense model sense)
  (gurobi-setintattr model 'ModelSense (gurobi-obj_sense->c sense)))

(define (getrowbound model lower)
  (let ((nr (getNumRows model))
        (c->s gurobi-c->constraint_sense))
    (map
      (if lower
        (lambda (s r) (lower-bound (c->s s) r))
        (lambda (s r) (upper-bound (c->s s) r)))
      (gurobi-getcharattrarray model 'Sense 0 nr)
      (gurobi-getdblattrarray model 'RHS 0 nr))))

(define (setrowbound model sense ix val)
  (gurobi-setcharattrelement model 'Sense ix (gurobi-constraint_sense->c sense))
  (gurobi-setdblattrelement model 'RHS ix val))

(define (getRowLower model)
  (getrowbound model #t))

(define (setRowLower model ix val)
  (setrowbound model '>= ix val))

(define (getRowUpper model)
  (getrowbound model #f))

(define (setRowUpper model ix val)
  (setrowbound model '<= ix val))

(define (getObjCoefficients model)
  (gurobi-getdblattrarray model 'Obj 0 (getNumCols model)))

(define (getColLower model)
  (gurobi-getdblattrarray model 'LB 0 (getNumCols model)))

(define (setColLower model ix val)
  (gurobi-setdblattrelement model 'LB ix val))

(define (getColUpper model)
  (gurobi-getdblattrarray model 'UB 0 (getNumCols model)))

(define (setColUpper model ix val)
  (gurobi-setdblattrelement model 'UB ix val))

(define (isInteger model ix)
  (equal? (gurobi-c->variable_type (gurobi-getcharattrelement model 'VType ix)) 'integer))

(define (setContinuous model ix)
  (gurobi-setcharattrelement model 'VType ix (gurobi-variable_type->c 'continuous)))

(define (setInteger model ix)
  (gurobi-setcharattrelement model 'VType ix (gurobi-variable_type->c 'integer)))

(define (setColTypes model types)
  (gurobi-setcharattrarray model 'VType 0 (getNumCols model)
                           (map gurobi-variable_type->c types)))

(define (getColTypes model)
  (map gurobi-c->variable_type (gurobi-getcharattrarray model 'VType 0 (getNumCols model))))

(define (getNumElements model)
  (gurobi-getintattr model 'NumNZs))

(define (getVectorStarts model)
  (let*-values ([(numvars) (getNumCols model)]
                [(starts ind val) (gurobi-getvars model 0 numvars)])
    starts))

(define (getIndices model)
  (let*-values ([(numvars) (getNumCols model)]
                [(starts ind val) (gurobi-getvars model 0 numvars)])
    ind))

(define (getElements model)
  (let*-values ([(numvars) (getNumCols model)]
                [(starts ind val) (gurobi-getvars model 0 numvars)])
    val))

(define (getObjValue model)
  (gurobi-getdblattr model 'ObjVal))

(define (printModel model unused)
  ; write ATTR format somehow to stdout?
  (error "Gurobi printModel Not yet implemented"))

(define (setPreprocess model p)
  (gurobi-setintparam (gurobi-getenv model) 'Presolve
    (case p [(automatic) -1] [(off) 0] [(conservative) 1] [(aggressive) 2])))

(define (setLogLevel model level)
  (gurobi-setintparam (gurobi-getenv model) 'OutputFlag (gurobi-log_level->c level)))

(define (solve model)
  (gurobi-optimize model)
  ; this is potentially different than CBC's results
  (hash-ref
    '#hash(
      (loaded          . before)
      (optimal         . finished)
      (infeasible      . errors)
      (inf-or-unbd     . errors)
      (unbounded       . finished)
      (cutoff          . stopped)
      (iteration-limit . stopped)
      (node-limit      . stopped)
      (time-limit      . stopped)
      (solution-limit  . stopped)
      (interrupted     . user-stopped)
      (numeric         . errors)
      (suboptimal      . finished)
      (inprogress      . before)
      (user-obj-limit  . stopped)
      (work-limit      . stopped)
      (mem-limit       . stopped))
    (gurobi-getmodelstatus model)))

(define (getNumRows model)
  (gurobi-getintattr model 'NumConstrs))

(define (getNumCols model)
  (gurobi-getintattr model 'NumVars))

(define (isAbandoned model)
  (equal? (gurobi-getmodelstatus model) 'numeric))

(define (isProvenOptimal model)
  (equal? (gurobi-getmodelstatus model) 'optimal))

(define (isProvenInfeasible model)
  (equal? (gurobi-getmodelstatus model) 'infeasible))

(define (getRowActivity model)
  (let* ((nr (getNumRows model))
         (rhs (gurobi-getdblattrarray model 'RHS 0 nr))
         (slack (gurobi-getdblattrarray model 'Slack 0 nr)))
    (map - rhs slack)))

(define (getColSolution model)
  (gurobi-getdblattrarray model 'X 0 (getNumCols model)))

(define (getIIS model)
  (gurobi-computeIIS model)
  (if (= 0 (gurobi-getintattr model 'IISMinimal))
    (error "IIS is somehow not minimal")
    (let* ((nr (getNumRows model))
           (whichRows (gurobi-getintattrarray model 'IISConstr 0 nr)))
      ; for now, not sure what else to do
      whichRows)))

(define gurobi
  (solver
    getVersion

    gurobi-Model?
    newModel
    gurobi-freemodel

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

    gurobi-obj_sense?
    gurobi-log_level?

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

    gurobi-model_status?

    solve

    getNumRows
    getNumCols

    isAbandoned
    isProvenOptimal
    isProvenInfeasible

    getRowActivity
    getColSolution
    ))
