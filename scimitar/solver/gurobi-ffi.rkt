#lang racket

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)
(require (for-syntax syntax/parse))
(require (only-in '#%foreign ctype-c->scheme ctype-scheme->c))

(require "string-enum.rkt")
(require "util.rkt")

(provide
    version
    (struct-out version-data)

    Env?
    loadenv
    emptyenv
    startenv
    freeenv

    geterrormsg
    getintparam
    setintparam
    getdblparam
    setdblparam
    getstrparam
    setstrparam

    Model?
    newmodel
    loadmodel
    getenv
    copymodel
    freemodel
    updatemodel
    reset
    resetmodel
    computeIIS

    readmodel
    write

    setobjective
    addvar
    addvars
    delvars
    addconstr
    addconstrs
    delconstrs
    addrangeconstr
    addrangeconstrs
    addsos
    delsos

    c->constraint_sense
    constraint_sense->c

    c->variable_type
    variable_type->c

    c->obj_sense
    obj_sense->c
    obj_sense?

    c->log_level
    log_level->c
    log_level?

    optimize

    model_status?

    getintattr
    setintattr
    getmodelstatus
    getintattrelement
    setintattrelement
    getintattrarray
    setintattrarray
    getcharattrelement
    setcharattrelement
    getcharattrarray
    setcharattrarray
    getdblattr
    setdblattr
    getdblattrelement
    setdblattrelement
    getdblattrarray
    setdblattrarray
    getstrattr
    setstrattr
    getstrattrelement
    setstrattrelement
    getstrattrarray
    setstrattrarray

    getnumnz
    getvars

    setsignal
    terminate)

(define-ffi-definer define-gurobi (ffi-lib "libgurobi"))

; Error codes
(define _error_code
  (_enum '(success = 0
           out-of-memory = 10001
           null-argument
           invalid-argument
           unknown-attribute
           data-not-available
           index-out-of-range
           unknown-parameter
           value-out-of-range
           no-license
           size-limit-exceeded
           callback
           file-read
           file-write
           numeric
           IIS-not-infeasible
           not-for-mip
           optimization-in-progress
           duplicates
           nodefile
           q-not-psd
           qcp-equality-constraint
           network
           job-rejected
           not-supported
           exceed-2b-nonzeros
           invalid-piecewise-obj
           updatemode-change
           cloud
           model-modification
           csworker
           tune-model-types
           security
           not-in-model = 20001
           failed-to-create-model
           internal)))

(struct version-data
  (major minor technical)
  #:transparent)

(define-gurobi version
  (_fun [majorP : (_box _int) = (box 0)]
        [minorP : (_box _int) = (box 0)]
        [technicalP : (_box _int) = (box 0)]
        -> _void
        -> (version-data (unbox majorP) (unbox minorP) (unbox technicalP)))
  #:c-id GRBversion)

(define-cpointer-type _Env)

(define (checked env-or-model status . vals)
  (if (equal? 'success status)
    (if (null? vals) (void) (car vals))
    (let ((env (if (Env? env-or-model)
                 env-or-model
                 (getenv env-or-model))))
      (error (format "Error ~a: ~a" status (geterrormsg env))))))

(define-gurobi freeenv
  (_fun [env : _Env] -> _void)
  #:wrap (deallocator)
  #:c-id GRBfreeenv)

(define-gurobi loadenv
  (_fun [env : (_box (_or-null _Env)) = (box #f)]
        [logfilename : _string]
        -> [status : _error_code]
        -> (checked (unbox env) status (unbox env)))
  #:wrap (allocator freeenv)
  #:c-id GRBloadenv)

(define-gurobi emptyenv
  (_fun [env : (_box (_or-null _Env)) = (box #f)]
        -> [status : _error_code]
        -> (checked (unbox env) status (unbox env)))
  #:wrap (allocator freeenv)
  #:c-id GRBemptyenv)

(define-gurobi startenv
  (_fun [env : _Env]
        -> [status : _error_code]
        -> (checked env status))  ; the FFI doesn't like returning other-than 1 value :^(
  #:wrap (allocator freeenv)
  #:c-id GRBstartenv)

(define-gurobi geterrormsg
  (_fun [env : _Env] -> _string)
  #:c-id GRBgeterrormsg)

(define _int_param
  (_string_enum
    '(; Termination parameters
      BarIterLimit
      SolutionLimit

      ; Licensing and Compute Server
      ServerTimeout
      CSPriority
      CSIdleTimeout
      CSTLSInsecure
      TSPort
      CSBatchMode
      CSClientLog
      WLSTokenDuration
      LicenseID

      ; Other parameters
      Aggregate
      AggFill
      ConcurrentMIP
      ConcurrentJobs
      DisplayInterval
      ListributedMIPJobs
      DualReductions
      IISMethod
      InfUnbdInfo
      JSONSolDetail
      LazyConstraints
      LogToConsole
      MIQCPMethod
      NonConvex
      NumericFocus
      OutputFlag
      PreCrush
      PreDepRow
      PreDual
      PrePasses
      PreQLinearize
      Presolve
      PreSOS1Encoding
      PreSOS2Encoding
      PreSparsify
      PreMIQCPForm
      QCPDual
      Record
      Seed
      SolutionTarget
      Threads
      TuneResults
      TuneCriterion
      TuneTrials
      TuneOutput
      TuneJobs
      TuneMetric
      UpdateMode
      ObjNumber
      MultiObjMethod
      MultiObjPre
      ScenarioNumber
      PoolSolutions
      PoolSearchMode
      IgnoreNames
      StartNumber
      PartitionPlace
      FuncPieces)))

(define-gurobi getintparam
  (_fun [env : _Env]
        [paramname : _int_param]
        [value : (_box _int) = (box 0)]
        -> [status : _error_code]
        -> (checked env status (unbox value)))
  #:c-id GRBgetintparam)

(define-gurobi setintparam
  (_fun [env : _Env]
        [paramname : _int_param]
        [value : _int]
        -> [status : _error_code]
        -> (checked env status))
  #:c-id GRBsetintparam)

(define _dbl_param
  (_string_enum
    '(; Termination parameters
      Cutoff
      IterationLimit
      NodeLimit
      TimeLimit
      WorkLimit
      MemLimit
      SoftMemLimit
      BestObjStop
      BestBdStop

      ; Tolerance parameters
      FeasibilityTol
      IntFeasTol
      MarkowitzTol
      MIPGap
      MIPGapAbs
      OptimalityTol
      PSDTol

      ; Licensing and Compute Server
      CSQueueTimeout
      WLSTokenRefresh

      ; Other parameters
      FeasRelaxBigM
      PreSOS1BigM
      PreSOS2BigM
      TuneTimeLimit
      TuneCleanup
      TuneTargetMIPGap
      TuneTargetTime
      PoolGap
      PoolGapAbs
      FuncPieceLength
      FuncPieceError
      FuncPieceRatio
      FuncMaxVal)))

(define-gurobi getdblparam
  (_fun [env : _Env]
        [paramname : _dbl_param]
        [value : (_box _double) = (box 0.0)]
        -> [status : _error_code]
        -> (checked env status (unbox value)))
  #:c-id GRBgetdblparam)

(define-gurobi setdblparam
  (_fun [env : _Env]
        [paramname : _dbl_param]
        [value : _double*]
        -> [status : _error_code]
        -> (checked env status))
  #:c-id GRBsetdblparam)

(define MAX_STRLEN 512)

(define _str_param
  (_string_enum
    '(; Licensing and Compute Server
      ComputeServer
      TokenServer
      ServerPassword
      CSRouter
      CSGroup
      CloudAccessID
      CloudSecretKey
      CloudPool
      CloudHost
      CSManager
      CSAuthToken
      CSAPIAccessID
      CSAPISecret
      Username
      CSAppName
      WLSAccessID
      WLSSecret
      WLSToken

      ; Other parameters
      LogFile
      ResultFile
      Dummy
      JobID)))

(define-gurobi getstrparam
  (_fun [env : _Env]
        [paramname : _str_param]
        [value : (_bytes o MAX_STRLEN)]
        -> [status : _error_code]
        -> (checked env status (bytes->string/utf-8 value)))
  #:c-id GRBgetstrparam)

(define-gurobi setstrparam
  (_fun [env : _Env]
        [paramname : _str_param]
        [value : _string]
        -> [status : _error_code]
        -> (checked env status))
  #:c-id GRBsetstrparam)

(define-cpointer-type _Model)

; Constraint senses
(define _constraint_sense
  (_enum `(<= = ,(char->integer #\<)
           >= = ,(char->integer #\>)
           =  = ,(char->integer #\=))
         _uint8))
(define (c->constraint_sense d)
  ((ctype-c->scheme _constraint_sense) d))
(define (constraint_sense->c s)
  ((ctype-scheme->c _constraint_sense) s))

; Variable types
(define _variable_type
  (_enum `(continuous = ,(char->integer #\C)
           binary = ,(char->integer #\B)
           integer = ,(char->integer #\I)
           semicont = ,(char->integer #\S)
           semiint = ,(char->integer #\N))
         _uint8))
(define (c->variable_type d)
  ((ctype-c->scheme _variable_type) d))
(define (variable_type->c s)
  ((ctype-scheme->c _variable_type) s))

; Objective sense
(define _obj_sense
  (_enum '(maximize = -1
           minimize = 1)))
(define (c->obj_sense d)
  ((ctype-c->scheme _obj_sense) d))
(define (obj_sense->c s)
  ((ctype-scheme->c _obj_sense) s))
(define (obj_sense? d)
  (symbols 'maximize 'minimize))

(define _log_level
  (_enum '(none = 0
           verbose)))
(define (c->log_level d)
  ((ctype-c->scheme _log_level) d))
(define (log_level->c s)
  ((ctype-scheme->c _log_level) s))
(define (log_level? d)
  (symbols 'none 'verbose))

(define-gurobi freemodel
  (_fun [model : _Model]
        -> [status : _error_code]
        -> (checked model status))
  #:wrap (deallocator)
  #:c-id GRBfreemodel)

(define-gurobi newmodel
  (_fun (env name obj lb ub vtype varnames) ::
        [env : _Env]
        [model : (_box (_or-null _Model)) = (box #f)]
        [name : _string]
        [numvars : _int = (maxlen lb ub obj varnames)]
        [obj : (_list i _double*)]
        [lb : (_list i _double*)]
        [ub : (_list i _double*)]
        [vtype : _string]
        [varnames : (_list i _string)]
        -> [status : _error_code]
        -> (checked env status (unbox model)))
  #:wrap (allocator freemodel)
  #:c-id GRBnewmodel)

; the lists below can fail with NULL pointer errors
(define (use-if-null ys xs)
  (if (null? xs) ys xs))

(define-gurobi loadmodel
  (_fun (env name objsense objcon obj sense rhs ixs elems lb ub vtype varnames constrnames) ::
        [env : _Env]
        [model : (_box (_or-null _Model)) = (box #f)]
        [name : _string]
        [numvars : _int = (maxlen lb ub obj ixs elems vtype)]
        [numconstrs : _int = (apply maxlen sense rhs constrnames elems)]
        [objsense : _obj_sense]
        [objcon : _double*]
        [obj : (_list i _double*) = (pad-to numvars 0.0 obj)]
        [sense : (_list i _constraint_sense)]
        [rhs : (_list i _double*)]
        [vbeg : (_list i _int) = (calc-starts (pad-to/null numvars '() elems))]
        [vlen : (_list i _int) = (map length (pad-to/null numvars '() elems))]
        [vind : (_list i _int) = (use-if-null '(0) (concat ixs))]
        [vval : (_list i _double*) = (use-if-null '(0) (concat elems))]
        [lb : (_list i _double*) = (pad-to numvars 0.0 lb)]
        [ub : (_list i _double*) = (pad-to numvars 0.0 ub)]
        [vtype : (_list i _variable_type)]
        [varnames : (_list i _string) = (pad-to numvars "" varnames)]
        [constrnames : (_list i _string) = (pad-to numconstrs "" constrnames)]
        -> [status : _error_code]
        -> (checked env status (unbox model)))
  #:wrap (allocator freemodel)
  #:c-id GRBloadmodel)

(define-gurobi readmodel
  (_fun [env : _Env]
        [filename : _string]
        [model : (_box (_or-null _Model)) = (box #f)]
        -> [status : _error_code]
        -> (checked env status (unbox model)))
  #:wrap (allocator freemodel)
  #:c-id GRBreadmodel)

(define-gurobi write
  (_fun [model : _Model]
        [filename : _string]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBwrite)

(define-gurobi getenv
  (_fun [model : _Model] -> (_or-null _Env))
  #:c-id GRBgetenv)

(define-gurobi copymodel
  (_fun [model : _Model] -> (_or-null _Model))
  #:wrap (allocator freemodel)
  #:c-id GRBcopymodel)

(define-gurobi updatemodel
  (_fun [model : _Model]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBupdatemodel)

(define-gurobi reset
  (_fun [model : _Model]
        [clearall : _bool]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBreset)

(define-gurobi resetmodel
  (_fun [model : _Model]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBresetmodel)

(define-gurobi computeIIS
  (_fun [model : _Model]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBcomputeIIS)

; TODO: replace with file:///opt/gurobi1002/armlinux64/docs/refman/specifying_multiple_object.html
(define-gurobi setobjective
  (_fun (model sense constant lind lval qrow qcol qval) ::
        [model : _Model]
        [sense : _obj_sense]
        [constant : _double*]
        [lnz : _int = (maxlen lind lval)]
        [lind : (_list i _int)]
        [lval : (_list i _double*)]
        [qnz : _int = (maxlen qrow qcol qval)]
        [qrow : (_list i _int)] ; qrow and qcol are the 1st and 2nd vars in each
        [qcol : (_list i _int)] ; quadratic term. x1*x2 <=> row[j]=1, col[j]=2
        [qval : (_list i _double*)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetobjective)

(define-gurobi addvar
  (_fun (model vind vval obj lb ub vtype varname) ::
        [model : _Model]
        [numnz : _int = (maxlen vind vval)]
        [vind : (_list i _int)]
        [vval : (_list i _double*)]
        [obj :  _double*]
        [lb : _double*]
        [ub : _double*]
        [vtype : _variable_type]
        [varname : _string]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBaddvar)

(define (sum-lengths xss)
  (foldr (lambda (xs a) (+ a (length xs))) 0 xss))

(define-gurobi addvars
  (_fun (model ixs elems obj lb ub vtype varnames) ::
        [model : _Model]
        [numvars : _int = (maxlen lb ub obj ixs vtype varnames elems)]
        [numnz : _int = (sum-lengths ixs)]
        [vbeg : (_list i _int) = (calc-starts elems)]
        [vind : (_list i _int) = (concat ixs)]
        [vval : (_list i _double*) = (concat elems)]
        [obj : (_list i _double*) = (pad-to numvars 0.0 obj)]
        [lb : (_list i _double*) = (pad-to numvars 0.0 lb)]
        [ub : (_list i _double*) = (pad-to numvars 0.0 ub)]
        [vtype : (_list i _variable_type)]
        [varnames : (_list i _string)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBaddvars)

(define-gurobi delvars
  (_fun (model ind) ::
        [model : _Model]
        [numdel : _int = (length ind)]
        [ind : (_list i _int)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBdelvars)

(define-gurobi addconstr
  (_fun (model cind cval sense rhs constrname) ::
        [model : _Model]
        [numnz : _int = (maxlen cind cval)]
        [cind : (_list i _int)]
        [cval : (_list i _double*)]
        [sense : _constraint_sense]
        [rhs : _double*]
        [constrname : _string]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBaddconstr)

(define-gurobi addconstrs
  (_fun (model ixs elems sense rhs constrnames) ::
        [model : _Model]
        [numconstrs : _int = (maxlen ixs elems sense rhs)]
        [numnz : _int = (sum-lengths elems)]
        [cbeg : (_list i _int) = (calc-starts elems)]
        [cind : (_list i _int) = (concat ixs)]
        [cval : (_list i _double*) = (concat elems)]
        [sense : (_list i _constraint_sense)]
        [rhs : (_list i _double*)]
        [constrnames : (_list i _string)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBaddconstrs)

(define-gurobi delconstrs
  (_fun (model ind) ::
        [model : _Model]
        [numdel : _int = (length ind)]
        [ind : (_list i _int)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBdelconstrs)

(define-gurobi addrangeconstr
  (_fun (model cind cval lower upper constrname) ::
        [model : _Model]
        [numnz : _int = (maxlen cind cval)]
        [cind : (_list i _int)]
        [cval : (_list i _double*)]
        [lower : _double*]
        [upper : _double*]
        [constrname : _string]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBaddrangeconstr)

(define-gurobi addrangeconstrs
  (_fun (model ixs elems lower upper constrnames) ::
        [model : _Model]
        [numconstrs : _int = (maxlen ixs elems lower upper constrnames)]
        [numnz : _int = (sum-lengths elems)]
        [cbeg : (_list i _int) = (calc-starts elems)]
        [cind : (_list i _int) = (concat ixs)]
        [cval : (_list i _double*) = (concat elems)]
        [lower : (_list i _double*)]
        [upper : (_list i _double*)]
        [constrnames : (_list i _string)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBaddrangeconstrs)

; SOS types
(define _sos_type
  (_enum '(sos-type1 = 1
           sos-type2 = 2)))

(define-gurobi addsos
  (_fun (model types ixs weights) ::
        [model : _Model]
        [numsos : _int = (maxlen types ixs weights)]
        [nummembers : _int = (sum-lengths weights)]
        [types : (_list i _sos_type)]
        [beg : (_list i _int) = (calc-starts (pad-to numsos '() weights))]
        [ind : (_list i _int) = (concat ixs)]
        [weight : (_list i _double*) = (concat weights)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBaddsos)

(define-gurobi delsos
  (_fun (model ind) ::
        [model : _Model]
        [numdel : _int = (length ind)]
        [ind : (_list i _int)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBdelsos)

(define-gurobi optimize
  (_fun [model : _Model]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBoptimize)

(define _int_attr
  (_string_enum
    '(; Model attributes
      NumConstrs
      NumVars
      NumSOS
      NumQConstrs
      NumGenConstrs
      NumNZs
      NumQNZs
      NumQCNZs
      NumIntVars
      NumBinVars
      NumPWLObjVars
      ModelSense
      IsMIP
      IsQP
      IsQCP
      IsMultiObj
      LicenseExpiration
      NumTagged
      Fingerprint

      ; Model solution attributes
      Status
      SolCount
      BarIterCount
      HasDualNorm
      ConcurrentWinMethod

      ; IIS
      IISMinimal)))

(define-gurobi getintattr
  (_fun [model : _Model]
        [attrname : _int_attr]
        [value : (_box _int) = (box 0)]
        -> [status : _error_code]
        -> (checked model status (unbox value)))
  #:c-id GRBgetintattr)

(define-gurobi setintattr
  (_fun [model : _Model]
        [attrname : _int_attr]
        [newvalue : _int]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetintattr)

; Model status codes
(define _model_status
  (_enum '(loaded = 1
           optimal        
           infeasible     
           inf-or-unbd    
           unbounded      
           cutoff         
           iteration-limit
           node-limit     
           time-limit     
           solution-limit 
           interrupted    
           numeric        
           suboptimal     
           inprogress     
           user-obj-limit 
           work-limit     
           mem-limit)))
(define (model_status? d)
  (symbols 'loaded 'optimal 'infeasible 'inf-or-unbd 'unbounded 'cutoff
           'iteration-limit 'node-limit 'time-limit 'solution-limit
           'interrupted 'numeric 'suboptimal 'inprogress
           'user-obj-limit 'work-limit 'mem-limit))

(define-gurobi getmodelstatus
  (_fun [model : _Model]
        [attrname : _symbol = 'Status]
        [value : (_box _model_status) = (box 'loaded)]
        -> [status : _error_code]
        -> (checked model status (unbox value)))
  #:c-id GRBgetintattr)

(define _int_array_attr
  (_string_enum
    '(; Variable attributes
      BranchPriority
      PWLObjCvx
      VarHintPri
      Partition
      PoolIgnore

      ; Constraint attributes
      Lazy

      ; Variable attributes related to the current solution
      VBasis

      ; Constraint attributes related to the current solution
      CBasis

      ; IIS
      IISLB
      IISUB
      IISConstr
      IISConstrForce
      )))

(define-gurobi getintattrelement
  (_fun [model : _Model]
        [attrname : _int_array_attr]
        [element : _int]
        [valueP : (_box _int) = (box 0)]
        -> [status : _error_code]
        -> (checked model status (unbox valueP)))
  #:c-id GRBgetintattrelement)

(define-gurobi setintattrelement
  (_fun [model : _Model]
        [attrname : _int_array_attr]
        [element : _int]
        [newvalue : _int]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetintattrelement)

(define-gurobi getintattrarray
  (_fun [model : _Model]
        [attrname : _int_array_attr]
        [first : _int]
        [len : _int]
        [vals : (_list o _int len)]
        -> [status : _error_code]
        -> (checked model status vals))
  #:c-id GRBgetintattrarray)

(define-gurobi setintattrarray
  (_fun [model : _Model]
        [attrname : _int_array_attr]
        [first : _int]
        [len : _int]
        [vals : (_list i _int)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetintattrarray)

(define _char_array_attr
  (_string_enum
    '(; Variable attributes
      VType
      ; Constraint attributes
      Sense)))

(define-gurobi getcharattrelement
  (_fun [model : _Model]
        [attrname : _char_array_attr]
        [element : _int]
        [valueP : (_box _uint8) = (box 0)]
        -> [status : _error_code]
        -> (checked model status (unbox valueP)))
  #:c-id GRBgetcharattrelement)

(define-gurobi setcharattrelement
  (_fun [model : _Model]
        [attrname : _char_array_attr]
        [element : _int]
        [newvalue : _uint8]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetcharattrelement)

(define-gurobi getcharattrarray
  (_fun [model : _Model]
        [attrname : _char_array_attr]
        [first : _int]
        [len : _int]
        [vals : (_list o _uint8 len)]
        -> [status : _error_code]
        -> (checked model status vals))
  #:c-id GRBgetcharattrarray)

(define-gurobi setcharattrarray
  (_fun [model : _Model]
        [attrname : _char_array_attr]
        [first : _int]
        [len : _int]
        [vals : (_list i _uint8)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetcharattrarray)

(define _dbl_attr
  (_string_enum
    '(; Model attributes
      DNumNZs
      ObjCon

      ; Model solution attributes
      Runtime
      Work
      ObjVal
      ObjBound
      ObjBoundC
      PoolObjBound
      PoolObjVal
      MIPGap
      IterCount
      NodeCount
      OpenNodeCount)))

(define-gurobi getdblattr
  (_fun [model : _Model]
        [attrname : _dbl_attr]
        [value : (_box _double) = (box 0.0)]
        -> [status : _error_code]
        -> (checked model status (unbox value)))
  #:c-id GRBgetdblattr)

(define-gurobi setdblattr
  (_fun [model : _Model]
        [attrname : _dbl_attr]
        [newvalue : _double*]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetdblattr)

(define _dbl_array_attr
  (_string_enum
    '(; Variable attributes
      LB
      UB
      Obj
      Start
      PStart
      VarHintVal

      ; Constraint attributes
      RHS
      DStart

      ; Variable attributes related to the current solution
      X
      Xn
      BarX
      RC
      VDualNorm

      ; Constraint attributes related to the current solution
      Pi
      QCPi
      Slack
      QCSlack
      CDualNorm

      ; LP sensitivity analysis
      SARHSLow
      SARHSUp

      ; Advanced simplex features
      FarkasDual
      )))

(define-gurobi getdblattrelement
  (_fun [model : _Model]
        [attrname : _dbl_array_attr]
        [element : _int]
        [valueP : (_box _double) = (box 0.0)]
        -> [status : _error_code]
        -> (checked model status (unbox valueP)))
  #:c-id GRBgetdblattrelement)

(define-gurobi setdblattrelement
  (_fun [model : _Model]
        [attrname : _dbl_array_attr]
        [element : _int]
        [newvalue : _double*]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetdblattrelement)

(define-gurobi getdblattrarray
  (_fun [model : _Model]
        [attrname : _dbl_array_attr]
        [first : _int]
        [len : _int]
        [vals : (_list o _double len)]
        -> [status : _error_code]
        -> (checked model status vals))
  #:c-id GRBgetdblattrarray)

(define-gurobi setdblattrarray
  (_fun [model : _Model]
        [attrname : _dbl_array_attr]
        [first : _int]
        [len : _int]
        [vals : (_list i _double*)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetdblattrarray)

(define _str_attr
  (_string_enum
    '(; Model attributes
      ModelName)))

(define-gurobi getstrattr
  (_fun [model : _Model]
        [attrname : _str_attr]
        [value : (_box _string) = (box "")]
        -> [status : _error_code]
        -> (checked model status (unbox value)))
  #:c-id GRBgetstrattr)

(define-gurobi setstrattr
  (_fun [model : _Model]
        [attrname : _str_attr]
        [newvalue : _string]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetstrattr)

(define _str_array_attr
  (_string_enum
    '(; Variable attributes
      VType
      VarName
      VTag

      ; Constraint attributes
      CTag
      ConstrName)))

(define-gurobi getstrattrelement
  (_fun [model : _Model]
        [attrname : _str_array_attr]
        [element : _int]
        [valueP : (_box _string) = (box "")]
        -> [status : _error_code]
        -> (checked model status (unbox valueP)))
  #:c-id GRBgetstrattrelement)

(define-gurobi setstrattrelement
  (_fun [model : _Model]
        [attrname : _str_array_attr]
        [element : _int]
        [newvalue : _string]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetstrattrelement)

(define-gurobi getstrattrarray
  (_fun [model : _Model]
        [attrname : _str_array_attr]
        [first : _int]
        [len : _int]
        [vals : (_list o _string len)]
        -> [status : _error_code]
        -> (checked model status vals))
  #:c-id GRBgetstrattrarray)

(define-gurobi setstrattrarray
  (_fun [model : _Model]
        [attrname : _str_array_attr]
        [first : _int]
        [len : _int]
        [vals : (_list i _string)]
        -> [status : _error_code]
        -> (checked model status))
  #:c-id GRBsetstrattrarray)

(define-gurobi getnumnz
  (_fun [model : _Model]
        [numnzP : (_box _int) = (box 0)]
        [vbeg : (_list i _int) = '()]
        [vind : (_list i _int) = '()]
        [vval : (_list i _double*) = '()]
        [start : _int]
        [len : _int]
        -> [status : _error_code]
        -> (checked model status (unbox numnzP)))
  #:c-id GRBgetvars)

(define (deref ptr)
  (if (cpointer? ptr)
    (ptr-ref ptr _int)
    (unbox ptr)))

(define-gurobi getvars-raw
  (_fun (model start len) ::
        [model : _Model]
        [numnzP : (_box _int) = (box (getnumnz model start len))]
        [vbeg : (_list o _int len)]
        [vind : (_list o _int (deref numnzP))]
        [vval : (_list o _double (deref numnzP))]
        [start : _int]
        [len : _int]
        -> [status : _error_code]   ; when numnzP is 0 gurobi returns junk pointer
        -> (checked model status `(,(if (zero? (deref numnzP)) (make-list len 0) vbeg) ,vind ,vval)))
  #:c-id GRBgetvars)

(define (getvars . xs)
  (apply values (apply getvars-raw xs)))

(define-gurobi setsignal
  (_fun [model : _Model] -> _void)
  #:c-id GRBsetsignal)

(define-gurobi terminate
  (_fun [model : _Model] -> _void)
  #:c-id GRBterminate)
