#lang racket

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require "util.rkt")

(provide
    version
    versionMajor
    versionMinor
    versionRelease

    Simplex?
    newModel
    deleteModel

    loadProblem
    setColumnName
    setRowName
    readMps
    writeMps
    addRows
    addColumns

    primalTolerance
    setPrimalTolerance
    dualTolerance
    setDualTolerance
    objectiveOffset
    setObjectiveOffset

    optimization_direction?
    log_level?

    getObjSense
    setObjSense
    getRowLower
    chgRowLower
    getRowUpper
    chgRowUpper
    getObjCoefficients
    getColLower
    chgColumnLower
    getColUpper
    chgColumnUpper
    getNumElements
    getVectorStarts
    getIndices
    getVectorLengths
    getElements
    getObjValue
    printModel
    setLogLevel

    status?
    pass?

    initialSolve
    dual

    saveModel
    restoreModel

    getNumRows
    getNumCols

    isAbandoned
    isProvenOptimal
    isProvenPrimalInfeasible
    isProvenDualInfeasible
    isPrimalObjectiveLimitReached
    isDualObjectiveLimitReached
    isIterationLimitReached

    getRowActivity
    getColSolution

    registerCallBack
    clearCallBack)

(define-ffi-definer define-clp (ffi-lib "libClp"))

(define-clp version
  (_fun -> _string)
  #:c-id Clp_Version)
(define-clp versionMajor
  (_fun -> _int)
  #:c-id Clp_VersionMajor)
(define-clp versionMinor
  (_fun -> _int)
  #:c-id Clp_VersionMinor)
(define-clp versionRelease
  (_fun -> _int)
  #:c-id Clp_VersionRelease)

(define-cpointer-type _Simplex)

(define-clp deleteModel
  (_fun [model : _Simplex] -> _void)
  #:wrap (deallocator)
  #:c-id Clp_deleteModel)
(define-clp newModel
  (_fun -> _Simplex)
  #:wrap (allocator deleteModel)
  #:c-id Clp_newModel)

(define-clp loadProblem
  (_fun (model ixs vals collb colub obj rowlb rowub) ::
        [model : _Simplex]
        [numcols : _int = (maxlen collb colub obj vals)]
        [numrows : _int = (apply maxlen rowlb rowub vals)]
        [start : (_list i _int) = (calc-starts (pad-to numcols '() vals))]
        [index : (_list i _int) = (concat ixs)]
        [value : (_list i _double*) = (concat vals)]
        [collb : (_list i _double*) = (pad-to numcols 0.0 collb)]
        [colub : (_list i _double*) = (pad-to numcols 0.0 colub)]
        [obj   : (_list i _double*) = (pad-to numcols 0.0 obj)]
        [rowlb : (_list i _double*) = (pad-to numrows 0.0 rowlb)]
        [rowub : (_list i _double*) = (pad-to numrows 0.0 rowub)] -> _void)
  #:c-id Clp_loadProblem)
(define-clp
    setColumnName
  (_fun [model : _Simplex] _int _string -> _void)
  #:c-id Clp_setColumnName)
(define-clp
    setRowName
  (_fun [model : _Simplex] _int _string -> _void)
  #:c-id Clp_setRowName)
(define-clp readMps
  (_fun [model : _Simplex] _string _bool _bool -> _int)
  #:c-id Clp_readMps)
(define-clp writeMps
  (_fun [model : _Simplex] _string _int _int _double* -> _int)
  #:c-id Clp_writeMps)
(define-clp addRows
  (_fun (model rowLower rowUpper ixs elems) ::
        [model : _Simplex]
        [number : _int = (maxlen rowLower rowUpper elems)]
        [rowLower  : (_list i _double*) = (pad-to number 0.0 rowLower)]
        [rowUpper  : (_list i _double*) = (pad-to number 0.0 rowUpper)]
        [rowStarts : (_list i _int) = (calc-starts elems)]
        [columns   : (_list i _int) = (concat ixs)]
        [elements  : (_list i _double*) = (concat elems)] -> _void)
  #:c-id Clp_addRows)
(define-clp addColumns
  (_fun (model columnLower columnUpper objective ixs elems) ::
        [model : _Simplex]
        [number : _int = (maxlen columnLower columnUpper objective elems)]
        [columnLower  : (_list i _double*) = (pad-to number 0.0 columnLower)]
        [columnUpper  : (_list i _double*) = (pad-to number 0.0 columnUpper)]
        [objective    : (_list i _double*) = (pad-to number 0.0 objective)]
        [columnStarts : (_list i _int) = (calc-starts elems)]
        [rows         : (_list i _int) = (concat ixs)]
        [elements     : (_list i _double*) = (concat elems)] -> _void)
  #:c-id Clp_addColumns)
(define-clp saveModel
  (_fun [model : _Simplex] _string -> _int)
  #:c-id Clp_saveModel)
(define-clp restoreModel
  (_fun [model : _Simplex] _string -> _int)
  #:c-id Clp_restoreModel)
(define-clp printModel
  (_fun [model : _Simplex] _string -> _void)
  #:c-id Clp_printModel)

(define-clp primalTolerance
  (_fun [model : _Simplex] -> _double)
  #:c-id Clp_primalTolerance)
(define-clp setPrimalTolerance
  (_fun [model : _Simplex] _double -> _void)
  #:c-id Clp_setPrimalTolerance)
(define-clp dualTolerance
  (_fun [model : _Simplex] -> _double)
  #:c-id Clp_dualTolerance)
(define-clp setDualTolerance
  (_fun [model : _Simplex] _double -> _void)
  #:c-id Clp_setDualTolerance)
(define-clp objectiveOffset
  (_fun [model : _Simplex] -> _double)
  #:c-id Clp_objectiveOffset)
(define-clp setObjectiveOffset
  (_fun [model : _Simplex] _double -> _void)
  #:c-id Clp_setObjectiveOffset)

(define _int_inexact
  (make-ctype
    _double
    (lambda (x) (exact->inexact x))
    (lambda (x) (inexact->exact x))))

(define _optimization_direction
  (_enum '(maximize = -1
           ignore = 0
           minimize = 1)
         _int_inexact))
(define (optimization_direction? d)
  (symbols 'maximize 'ignore 'minimize))

(define _log_level
  (_enum '(none = 0
           final
           factorizations
           plus-a-bit-more
           verbose)))
(define (log_level? d)
  (symbols 'none 'final 'factorizations 'plus-a-bit-more 'verbose))

(define-clp getObjSense
  (_fun [model : _Simplex] -> _optimization_direction)
  #:c-id Clp_getObjSense)
(define-clp setObjSense
  (_fun [model : _Simplex] _optimization_direction -> _void)
  #:c-id Clp_setObjSense)
(define-clp getRowLower
  (_fun [model : _Simplex] -> (_list o _double (getNumRows model)))
  #:c-id Clp_getRowLower)
(define-clp chgRowLower
  (_fun [model : _Simplex] (_list i _double*) -> _void)
  #:c-id Clp_chgRowLower)
(define-clp getRowUpper
  (_fun [model : _Simplex] -> (_list o _double (getNumRows model)))
  #:c-id Clp_getRowUpper)
(define-clp chgRowUpper
  (_fun [model : _Simplex] (_list i _double*) -> _void)
  #:c-id Clp_chgRowUpper)
(define-clp getObjCoefficients
  (_fun [model : _Simplex] -> (_list o _double (getNumCols model)))
  #:c-id Clp_getObjCoefficients)
(define-clp getColLower
  (_fun [model : _Simplex] -> (_list o _double (getNumCols model)))
  #:c-id Clp_getColLower)
(define-clp chgColumnLower
  (_fun [model : _Simplex] (_list i _double*) -> _void)
  #:c-id Clp_chgColumnLower)
(define-clp getColUpper
  (_fun [model : _Simplex] -> (_list o _double (getNumCols model)))
  #:c-id Clp_getColUpper)
(define-clp chgColumnUpper
  (_fun [model : _Simplex] (_list i _double*) -> _void)
  #:c-id Clp_chgColumnUpper)
(define-clp getNumElements
  (_fun [model : _Simplex] -> _int)
  #:c-id Clp_getNumElements)
(define-clp getVectorStarts
  (_fun [model : _Simplex] -> (_list o _int (getNumCols model)))
  #:c-id Clp_getVectorStarts)
(define-clp getIndices
  (_fun [model : _Simplex] -> (_list o _int (getNumElements model)))
  #:c-id Clp_getIndices)
(define-clp getVectorLengths
  (_fun [model : _Simplex] -> (_list o _int (getNumCols model)))
  #:c-id Clp_getVectorLengths)
(define-clp getElements
  (_fun [model : _Simplex] -> (_list o _double (getNumElements model)))
  #:c-id Clp_getElements)
(define-clp getObjValue
  (_fun [model : _Simplex] -> _double)
  #:c-id Clp_getObjValue)
(define-clp setLogLevel
  (_fun [model : _Simplex] _log_level -> _void)
  #:c-id Clp_setLogLevel)

(define _status
  (_enum '(finished = 0
           finished
           finished
           stopped
           errors
           user-stopped
           unknown)
         #:unknown (lambda (x) 'unknown)))
(define (status? d)
  (symbols 'finished 'stopped 'errors 'user-stopped 'unknown))

(define _pass
  (_enum '(initial = 0
           values-pass
           cleanup)))
(define (pass? d)
  (symbols 'initial 'values-pass 'cleanup))

(define-clp initialSolve
  (_fun [model : _Simplex] -> _status)
  #:c-id Clp_initialSolve)
(define-clp dual
  (_fun [model : _Simplex] _pass -> _int)
  #:c-id Clp_dual)

(define-clp getNumRows
  (_fun [model : _Simplex] -> _int)
  #:c-id Clp_getNumRows)
(define-clp getNumCols
  (_fun [model : _Simplex] -> _int)
  #:c-id Clp_getNumCols)

(define-clp isAbandoned
  (_fun [model : _Simplex] -> _bool)
  #:c-id Clp_isAbandoned)
(define-clp isProvenOptimal
  (_fun [model : _Simplex] -> _bool)
  #:c-id Clp_isProvenOptimal)
(define-clp isProvenPrimalInfeasible
  (_fun [model : _Simplex] -> _bool)
  #:c-id Clp_isProvenPrimalInfeasible)
(define-clp isProvenDualInfeasible
  (_fun [model : _Simplex] -> _bool)
  #:c-id Clp_isProvenDualInfeasible)
(define-clp isPrimalObjectiveLimitReached
  (_fun [model : _Simplex] -> _bool)
  #:c-id Clp_isPrimalObjectiveLimitReached)
(define-clp isDualObjectiveLimitReached
  (_fun [model : _Simplex] -> _bool)
  #:c-id Clp_isDualObjectiveLimitReached)
(define-clp isIterationLimitReached
  (_fun [model : _Simplex] -> _bool)
  #:c-id Clp_isIterationLimitReached)

(define-clp getRowActivity
  (_fun [model : _Simplex] -> (_list o _double (getNumRows model)))
  #:c-id Clp_getRowActivity)
(define-clp getColSolution
  (_fun [model : _Simplex] -> (_list o _double (getNumCols model)))
  #:c-id Clp_getColSolution)

(define callback
  (_fun [model : _Simplex]
        [msgno : _int]
        [ndouble : _int]
        [dvec : _pointer]
        [nint : _int]
        [ivec : _pointer]
        [nchar : _int]
        [cvec : _pointer] -> _void))
(define-clp registerCallBack-raw
  (_fun [model : _Simplex] callback -> _void)
  #:c-id Clp_registerCallBack)
(define (registerCallBack model callback)
  (registerCallBack-raw model
    (lambda (m mn nd dv ni iv nc cv)
      (let ((dv (cblock->list dv _double* nd))
            (iv (cblock->list iv _int ni))
            (cv (cblock->list cv _string nc)))
        (callback m mn dv iv cv)))))
(define-clp clearCallBack
  (_fun [model : _Simplex] -> _void)
  #:c-id Clp_clearCallBack)
