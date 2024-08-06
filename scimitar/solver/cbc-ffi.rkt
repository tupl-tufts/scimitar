#lang racket

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)
(require (rename-in racket/contract [-> -->]))

(require "util.rkt")

(provide
    getVersion

    Model?
    newModel
    deleteModel
    clone

    loadProblem
    setColName
    setRowName
    readMps
    readLp
    writeMps
    writeLp

    addCol
    addRow
    addSOS

    optimization_direction?
    log_level?

    getRowNz
    getRowIndices
    getRowCoeffs
    getRowRHS
    getRowSense

    getColNz
    getColIndices
    getColCoeffs

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
    getNumElements
    getVectorStarts
    getIndices
    getElements
    getObjValue
    printModel

    setParameter
    setPreprocess
    setAllowableGap
    setLogLevel

    status?

    status
    secondaryStatus

    solve

    getNumRows
    getNumIntegers
    getNumCols

    isAbandoned
    isProvenOptimal
    isProvenInfeasible
    isContinuousUnbounded
    isSolutionLimitReached

    getRowActivity
    getColSolution

    registerCallBack
    clearCallBack)

(define-ffi-definer define-cbc (ffi-lib "libCbcSolver"))

(define-cbc getVersion
  (_fun -> _string)
  #:c-id Cbc_getVersion)

(define-cpointer-type _Model)

(define-cbc deleteModel
  (_fun [model : _Model] -> _void)
  #:wrap (deallocator)
  #:c-id Cbc_deleteModel)
(define-cbc newModel
  (_fun -> _Model)
  #:wrap (allocator deleteModel)
  #:c-id Cbc_newModel)
(define-cbc clone
  (_fun [model : _Model] -> _Model)
  #:wrap (allocator deleteModel)
  #:c-id Cbc_clone)

(define-cbc loadProblem
  (_fun (model ixs vals collb colub obj rowlb rowub) ::
        [model : _Model]
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
  #:c-id Cbc_loadProblem)
(define-cbc
    setColName
  (_fun [model : _Model] _int _string -> _void)
  #:c-id Cbc_setColName)
(define-cbc
    setRowName
  (_fun [model : _Model] _int _string -> _void)
  #:c-id Cbc_setRowName)
(define-cbc readMps
  (_fun [model : _Model] _string -> _int)
  #:c-id Cbc_readMps)
(define-cbc readLp
  (_fun [model : _Model] _string -> _int)
  #:c-id Cbc_readLp)
(define-cbc writeMps
  (_fun [model : _Model] _string -> _void)
  #:c-id Cbc_writeMps)
(define-cbc writeLp
  (_fun [model : _Model] _string -> _void)
  #:c-id Cbc_writeLp)
(define-cbc printModel
  (_fun [model : _Model] _string -> _void)
  #:c-id Cbc_printModel)

(define _sense
  (_enum `(<= = ,(char->integer #\L)
           >= = ,(char->integer #\G)
           =  = ,(char->integer #\E)
           ranged = ,(char->integer #\R))))

(define-cbc addCol
  (_fun (model name lb ub obj rows coefs) ::
        [model : _Model]
        [name : _string]
        [lb : _double*]
        [ub : _double*]
        [obj : _double*]
        [isInteger : _bool = (not (equal? 'continuous (range-class lb ub)))]
        [nz : _int = (maxlen rows coefs)]
        [rows : (_list i _int)]
        [coefs : (_list i _double*)] -> _void)
  #:c-id Cbc_addCol)
(define-cbc addRow
  (_fun (model name cols coefs sense rhs) ::
        [model : _Model]
        [name : _string]
        [nz : _int = (maxlen cols coefs)]
        [cols : (_list i _int)]
        [coefs : (_list i _double*)]
        [sense : _sense]
        [rhs : _double*] -> _void)
  #:c-id Cbc_addRow)
(define-cbc addSOS
  (_fun (model rowStarts colIndices weights type) ::
        [model : _Model]
        [numRows : _int = (maxlen rowStarts colIndices)]
        [rowStarts : (_list i _int)]
        [colIndices : (_list i _int)]
        [weights : (_list i _double*)]
        [type : _int] -> _void)
  #:c-id Cbc_addSOS)

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
           normal
           plus-a-bit-more
           verbose)))
(define (log_level? d)
  (symbols 'none 'normal 'plus-a-bit-more 'verbose))

(define-cbc getRowNz
  (_fun [model : _Model] _int -> _int)
  #:c-id Cbc_getRowNz)
(define-cbc getRowIndices
  (_fun [model : _Model] _int -> (_list o _int (getRowNz model)))
  #:c-id Cbc_getRowIndices)
(define-cbc getRowCoeffs
  (_fun [model : _Model] _int -> (_list o _double (getRowNz model)))
  #:c-id Cbc_getRowCoeffs)
(define-cbc getRowRHS
  (_fun [model : _Model] _int -> _double)
  #:c-id Cbc_getRowRHS)
(define-cbc getRowSense
  (_fun [model : _Model] _int -> _sense)
  #:c-id Cbc_getRowSense)

(define-cbc getColNz
  (_fun [model : _Model] _int -> _int)
  #:c-id Cbc_getColNz)
(define-cbc getColIndices
  (_fun [model : _Model] _int -> (_list o _int (getColNz model)))
  #:c-id Cbc_getColIndices)
(define-cbc getColCoeffs
  (_fun [model : _Model] _int -> (_list o _double (getColNz model)))
  #:c-id Cbc_getColCoeffs)

(define-cbc getObjSense
  (_fun [model : _Model] -> _optimization_direction)
  #:c-id Cbc_getObjSense)
(define-cbc setObjSense
  (_fun [model : _Model] _optimization_direction -> _void)
  #:c-id Cbc_setObjSense)
(define-cbc getRowLower
  (_fun [model : _Model] -> (_list o _double (getNumRows model)))
  #:c-id Cbc_getRowLower)
(define-cbc setRowLower
  (_fun [model : _Model] _int _double* -> _void)
  #:c-id Cbc_setRowLower)
(define-cbc getRowUpper
  (_fun [model : _Model] -> (_list o _double (getNumRows model)))
  #:c-id Cbc_getRowUpper)
(define-cbc setRowUpper
  (_fun [model : _Model] _int _double* -> _void)
  #:c-id Cbc_setRowUpper)
(define-cbc getObjCoefficients
  (_fun [model : _Model] -> (_list o _double (getNumCols model)))
  #:c-id Cbc_getObjCoefficients)
(define-cbc getColLower
  (_fun [model : _Model] -> (_list o _double (getNumCols model)))
  #:c-id Cbc_getColLower)
(define-cbc setColLower
  (_fun [model : _Model] _int _double* -> _void)
  #:c-id Cbc_setColLower)
(define-cbc getColUpper
  (_fun [model : _Model] -> (_list o _double (getNumCols model)))
  #:c-id Cbc_getColUpper)
(define-cbc setColUpper
  (_fun [model : _Model] _int _double* -> _void)
  #:c-id Cbc_setColUpper)
(define-cbc isInteger
  (_fun [model : _Model] _int -> _bool)
  #:c-id Cbc_isInteger)
(define-cbc setContinuous
  (_fun [model : _Model] _int -> _void)
  #:c-id Cbc_setContinuous)
(define-cbc setInteger
  (_fun [model : _Model] _int -> _void)
  #:c-id Cbc_setInteger)
(define-cbc getNumElements
  (_fun [model : _Model] -> _int)
  #:c-id Cbc_getNumElements)
(define-cbc getVectorStarts
  (_fun [model : _Model] -> (_list o _int (getNumCols model)))
  #:c-id Cbc_getVectorStarts)
(define-cbc getIndices
  (_fun [model : _Model] -> (_list o _int (getNumElements model)))
  #:c-id Cbc_getIndices)
(define-cbc getElements
  (_fun [model : _Model] -> (_list o _double (getNumElements model)))
  #:c-id Cbc_getElements)
(define-cbc getObjValue
  (_fun [model : _Model] -> _double)
  #:c-id Cbc_getObjValue)

(define-cbc setParameter
  (_fun [model : _Model] _string _string -> _void)
  #:c-id Cbc_setParameter)
(define/contract (setPreprocess m v)
  (--> Model? (or/c 'off 'on 'save 'equal 'sos 'trysos 'equalall 'strategy
                    'aggregate 'forcesos 'stopaftersaving 'equalallstop) void?)
  (setParameter m "preprocess" (symbol->string v)))
(define-cbc setAllowableGap
  (_fun [model : _Model] _double* -> _void)
  #:c-id Cbc_setAllowableGap)
(define-cbc setLogLevel
  (_fun [model : _Model] _log_level -> _void)
  #:c-id Cbc_setLogLevel)

(define _status
  (_enum '(before = -1
           finished
           stopped
           errors
           unknown
           unknown
           user-stopped
           unknown)
         #:unknown (lambda (x) 'unknown)))
(define (status? d)
  (symbols 'before 'finished 'stopped 'errors 'user-stopped 'unknown))

(define _secondaryStatus
  (_enum '(unset = -1
           completed
           linear-relaxation-infeasible
           stopped-on-gap
           stopped-on-nodes
           stopped-on-time
           stopped-on-user-event
           stopped-on-solutions
           linear-relaxation-unbounded
           stopped-on-iteration-limit)
         #:unknown (lambda (x) 'unknown)))

(define-cbc status
  (_fun [model : _Model] -> _status)
  #:c-id Cbc_status)
(define-cbc secondaryStatus
  (_fun [model : _Model] -> _secondaryStatus)
  #:c-id Cbc_secondaryStatus)

(define-cbc solve
  (_fun [model : _Model] -> _status)
  #:c-id Cbc_solve)

(define-cbc getNumRows
  (_fun [model : _Model] -> _int)
  #:c-id Cbc_getNumRows)
(define-cbc getNumIntegers
  (_fun [model : _Model] -> _int)
  #:c-id Cbc_getNumIntegers)
(define-cbc getNumCols
  (_fun [model : _Model] -> _int)
  #:c-id Cbc_getNumCols)

(define-cbc isAbandoned
  (_fun [model : _Model] -> _bool)
  #:c-id Cbc_isAbandoned)
(define-cbc isProvenOptimal
  (_fun [model : _Model] -> _bool)
  #:c-id Cbc_isProvenOptimal)
(define-cbc isProvenInfeasible
  (_fun [model : _Model] -> _bool)
  #:c-id Cbc_isProvenInfeasible)
(define-cbc isContinuousUnbounded
  (_fun [model : _Model] -> _bool)
  #:c-id Cbc_isContinuousUnbounded)
(define-cbc isSolutionLimitReached
  (_fun [model : _Model] -> _bool)
  #:c-id Cbc_isSolutionLimitReached)

(define-cbc getRowActivity
  (_fun [model : _Model] -> (_list o _double (getNumRows model)))
  #:c-id Cbc_getRowActivity)
(define-cbc getColSolution
  (_fun [model : _Model] -> (_list o _double (getNumCols model)))
  #:c-id Cbc_getColSolution)

(define callback
  (_fun [model : _Model]
        [msgno : _int]
        [ndouble : _int]
        [dvec : _pointer]
        [nint : _int]
        [ivec : _pointer]
        [nchar : _int]
        [cvec : _pointer] -> _void))
(define-cbc registerCallBack-raw
  (_fun [model : _Model] callback -> _void)
  #:c-id Cbc_registerCallBack)
(define (registerCallBack model callback)
  (registerCallBack-raw model
    (lambda (m mn nd dv ni iv nc cv)
      (let ((dv (cblock->list dv _double* nd))
            (iv (cblock->list iv _int ni))
            (cv (cblock->list cv _string nc)))
        (callback m mn dv iv cv)))))
(define-cbc clearCallBack
  (_fun [model : _Model] -> _void)
  #:c-id Cbc_clearCallBack)
