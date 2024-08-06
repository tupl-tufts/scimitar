#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "../ir/compiler.rkt")
(require "../ir/grammar.rkt")
(require "../ir/translate.rkt")
(require "../ir/util.rkt")
(require "../prim/op.ir.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide (prefix-out scimitar- desugar))

(define (desugar e)
  (define/match (phase1 e)
    [((scimitar-solve dir res obj locals problems))
     (scimitar-solve dir res obj locals (map phase1 problems))]
    [((scimitar-typed e ty))
     (scimitar-typed (phase1 e) ty)]
    [((scimitar-tuple es))
     (scimitar-tuple (map phase1 es))]
    [((scimitar-switch op e es))
     (scimitar-switch op (phase1 e) (map phase1 es))]
    [((scimitar-app f args))
     (scimitar-app (phase1 f) (phase1 args))]
    [((scimitar-lambda ps body))
     (scimitar-lambda ps (phase1 body))]
    [((scimitar-fix fs e))
     (scimitar-fix (map-cadr phase1 fs) (phase1 e))]
    [((scimitar-let bs e))
     (let* ((ps (sym-val-list->sym-val (map car bs)))
            (as (map (compose phase1 cadr) bs))
            (a (scimitar-typed-list->scimitar-typed as))
            (e (phase1 e))
            (tyf (polyty (scimitar-typed-ty a) '() (scimitar-typed-ty e))))
       (scimitar-app (scimitar-typed (scimitar-lambda ps e) tyf) a))]
    [((scimitar-let* bs e))
     (let ((ty (scimitar-typed-ty e))
           (e (scimitar-typed-e e)))
       (phase1 (foldr (lambda (b acc) (scimitar-let `(,b) (scimitar-typed acc ty))) e bs)))]
    [((scimitar-begin es))
     (if (null? es)
       (scimitar-val '())
       (phase1
         (scimitar-let* (map (lambda (e) `(,(gensym 'begin) ,e)) (init es))
                        (last es))))]
    [((scimitar-plus e1 e2))
     (scimitar-plus (phase1 e1) (phase1 e2))]
    [((scimitar-minus e1 e2))
     (scimitar-minus (phase1 e1) (phase1 e2))]
    [((scimitar-mul e1 e2))
     (scimitar-mul (phase1 e1) (phase1 e2))]
    [((scimitar-lt e1 e2))
     (scimitar-lt (phase1 e1) (phase1 e2))]
    [((scimitar-eq e1 e2))
     (scimitar-eq (phase1 e1) (phase1 e2))]
    [((scimitar-gt e1 e2))
     (scimitar-gt (phase1 e1) (phase1 e2))]
    [((scimitar-ref vec ix))
     (scimitar-ref (phase1 vec) (phase1 ix))]
    [((scimitar-if eg et ef))
     (scimitar-if (phase1 eg) (phase1 et) (phase1 ef))]
    [((scimitar-for i en em eb))
     (scimitar-for i (phase1 en) (phase1 em) (phase1 eb))]
    [((scimitar-sum i en em eb))
     (scimitar-sum i (phase1 en) (phase1 em) (phase1 eb))]
    [((scimitar-constraint lhs ineq rhs))
     (scimitar-constraint (phase1 lhs) ineq (phase1 rhs))]
    [(e) e])

  (struct/contract
    phase2-bothex
    ([scexpr scimitar-typed?]
     [irexpr ir-expr?])
    #:transparent)

  (struct/contract
    phase2-scex scimitar-expr
    ([expr (or/c ir-expr? phase2-bothex?)]
     [Gamma-l env?]
     [cs (listof ir-constraint?)])
    #:transparent)

  (struct/contract
    phase2-irex ir-expr
    ([expr (or/c scimitar-typed? phase2-bothex?)])
    #:transparent)

  (struct/contract
    ir-typed ir-expr
    ([e ir-expr?]
     [ty ty?]
     [inside-solve boolean?])
    #:transparent)

  (define (make-scex e Gamma-l cs)
    (if (scimitar-expr? e)
      e
      (phase2-scex e Gamma-l cs)))

  (define (make-irex e)
    (if (ir-expr? e)
      e
      (phase2-irex e)))

  (define (env-checked-union-key . es)
    (apply env-union-key es
           #:combine (lambda (v u) (if (equal? v u) v (error (format "While desugaring, key got conflicting values:~n~v~n~v" v u))))))

  (define inside-solve (box #f))
  (define (inside-solve?) (unbox inside-solve))
  (define (inside-solve!) (set-box! inside-solve #t))
  (define (outside-solve!) (set-box! inside-solve #f))

  (define (phase2 e)
    (define/match (go e)
      [((scimitar-typed (scimitar-solve dir res obj locals problems) ty))
       (inside-solve!)
       (let-values ([(problems Gs css) (for/lists (problems Gs css) ([problem problems]) (go problem))])
         (outside-solve!)
         (values (scimitar-typed (scimitar-solve dir res obj locals (map make-scex problems Gs css)) ty)
                 (env-empty)
                 '()))]
      [((scimitar-typed (scimitar-var x) _))
       (values (phase2-bothex e (ir-var x))
               (env-empty)
               '())]
      [((or (scimitar-typed (scimitar-val v) _)
            (scimitar-typed (scimitar-poly v) _)))
       (values (phase2-bothex e (sym-val->ir-val v))
               (env-empty)
               '())]
      [((scimitar-typed (scimitar-tuple es) ty))
       (let-values ([(es Gs css) (for/lists (es Gs css) ([e es]) (go e))])
         (values (phase2-bothex
                   (scimitar-typed (scimitar-tuple (map make-scex es Gs css)) ty)
                   (ir-tuple (map make-irex es)))
                 (apply env-checked-union-key Gs)
                 (concat css)))]
      [((scimitar-typed (scimitar-switch op e es) ty))
       (let-values ([(e G cs) (go e)]
                    [(es Gs css) (for/lists (es Gs css) ([e es]) (go e))])
         (values (scimitar-typed (scimitar-switch op (make-scex e G cs) (map make-scex es Gs css)) ty)
                 (env-empty)
                 '()))]
      [((scimitar-typed (scimitar-app f args) ty))
       (let*-values ([(f Gf fcs) (go f)]
                     [(args Ga acs) (go args)]
                     [(sc-app) (scimitar-typed (scimitar-app (make-scex f Gf fcs) (make-scex args Ga acs)) ty)]
                     [(G) (env-checked-union-key Gf Ga)])
         ;; Naively, this should always be able to produce an ir-poly-app, but outside
         ;; of a solve that violates the order of operations in sequential code
         (if (inside-solve?)
           (values (phase2-bothex sc-app (ir-poly-app (make-irex f) (make-irex args))) G (append fcs acs))
           (values sc-app (env-empty) '())))]
      [((scimitar-typed (scimitar-lambda ps body) ty))
       (let-values ([(body G cs) (go body)])
         (values (scimitar-typed (scimitar-lambda ps (make-scex body G cs)) ty)
                 (env-empty)
                 '()))]
      [((scimitar-typed (scimitar-fix fs e) ty))
       (let-values ([(fs Gs css) (for/lists (fs Gs css) ([f fs])
                                   (let-values ([(l G cs) (go (cadr f))])
                                     (values `(,(car f) ,l) G cs)))]
                    [(e G cs) (go e)])
         (values (scimitar-typed
                   (scimitar-fix (map (lambda (f G cs) `(,(car f) ,(make-scex (cadr f) G cs)))
                                      fs Gs css)
                                 (make-scex e G cs))
                   ty)
                 (env-empty)
                 '()))]
      [((scimitar-typed (scimitar-num n) ty))
       (values (phase2-bothex
                 (scimitar-typed (scimitar-val (vec-dense n ty)) ty)
                 (ir-num n))
               (env-empty)
               '())]
      [((scimitar-typed (or (scimitar-plus e1 e2)
                            (scimitar-minus e1 e2)
                            (scimitar-mul e1 e2)
                            (scimitar-ref e1 e2)) ty))
       (let-values ([(op) (match (scimitar-typed-e e)
                            [(? scimitar-plus?) ir-add]
                            [(? scimitar-minus?) (lambda (e1 e2) (ir-sub e1 e2))]
                            [(? scimitar-mul?) (if (constty? (scimitar-typed-ty e1))
                                                 ir-scmul ir-var-mul)]
                            [(? scimitar-ref?) ir-vec-ix])]
                    [(e1 G1 cs1) (go e1)]
                    [(e2 G2 cs2) (go e2)])
         (values (ir-typed (op (make-irex e1) (make-irex e2)) ty (inside-solve?))
                 (env-checked-union-key G1 G2)
                 (append cs1 cs2)))]
      [((scimitar-typed (or (scimitar-lt e1 e2)
                            (scimitar-eq e1 e2)
                            (scimitar-gt e1 e2)) _))
       (let*-values ([(tye) (ty-least-upper-bound
                              (scimitar-typed-ty e1)
                              (scimitar-typed-ty e2))]
                     [(cmp) (op-cmp tye)]
                     [(tyc) (poly-ty cmp)]
                     [(_ __ t< t= t> t/) (apply values (ty->ty-list (polyty-dom tyc)))]
                     [(x<0 x=0 x>0 x/0) (values (gensym '<0) (gensym '=0) (gensym '>0) (gensym '/0))]
                     [(<0 =0 >0 /0) (values (ir-var x<0) (ir-var x=0) (ir-var x>0) (ir-var x/0))]
                     [(op) (match (scimitar-typed-e e)
                             [(? scimitar-lt?) (ir-typed <0 t< (inside-solve?))]
                             [(? scimitar-eq?) (ir-typed =0 t= (inside-solve?))]
                             [(? scimitar-gt?) (ir-typed >0 t> (inside-solve?))])]
                     [(e1 G1 cs1) (go e1)]
                     [(e2 G2 cs2) (go e2)]
                     [(ep) (ir-typed
                             (ir-poly-app
                               (ir-poly cmp)
                               (ir-tuple
                                 `(,(ir-cast (make-irex e1) tye) ,(ir-cast (make-irex e2) tye) ,<0 ,=0 ,>0 ,/0)))
                             (polyty-cod tyc)
                             (inside-solve?))]
                     [(cs) `(,(ir-constraint (ir-unit) '= ep))])
         (values op
                 (env-checked-union-key (env `((,x<0 ,t<) (,x=0 ,t/) (,x>0 ,t>) (,x/0 ,t/))) G1 G2)
                 (append cs cs1 cs2)))]
      [((scimitar-typed (scimitar-if eg et ef) ty))
       (let*-values ([(tyg) (scimitar-typed-ty eg)]
                     [(eg2 Gg csg) (go eg)]
                     [(et2 Gt cst) (go et)]
                     [(ef2 Gf csf) (go ef)])
         (define (op ty)
           (define-ir
             (ite ((: c (bitty)) (: v_t ty) (: v_f ty)) (: r ty))
             (= r (+ (* c v_t) (* (- 1 c) v_f))))
           ite)
         (define (check-for-constraints fmap p? e)
           (define has-constraints (box #f))
           (fmap p?  (lambda (e) (set-box! has-constraints #t) e) e)
           (unbox has-constraints))
         ; If there are any constraints either from processing et or ef, or for
         ; traversing their results, we must use the general switch case, which
         ; deals with constraints because constraints have to be dynamically
         ; enabled or disabled.  Else, we can optimize by directly lowering ITE here.
         (if (or (not (null? cst)) (not (null? csf))
                 (let-values ([(et3 etat) (traverse (make-irex et2))]
                              [(ef3 etaf) (traverse (make-irex ef2))])
                   (or (check-for-constraints ir-fmap (or/c ir-constraint? ir-poly-app?) et3)
                       (check-for-constraints ir-fmap (or/c ir-constraint? ir-poly-app?) ef3)
                       (ormap (curry check-for-constraints scimitar-fmap (or/c scimitar-infeasible? scimitar-app?)) (env-range etat))
                       (ormap (curry check-for-constraints scimitar-fmap (or/c scimitar-infeasible? scimitar-app?)) (env-range etaf)))))
           (go (scimitar-typed (scimitar-switch op eg `(,et ,ef)) ty))
           ; We Can only do this if everything is IR.  otherwise both
           ; et and ef are evaluated instead of being conditional.
           (let* ((c (gensym 'c))
                  (cs `(,(ir-constraint (ir-var c) '= (make-irex eg2))))
                  (ite (ir-add (ir-var-mul (ir-var c) (make-irex et2))
                               (ir-var-mul (ir-sub (ir-num 1) (ir-var c))
                                           (make-irex ef2)))))
             (values (ir-typed ite ty (inside-solve?))
                     (env-checked-union-key (env `((,c ,tyg))) Gg Gt Gf)
                     (append cs csg cst csf)))))]
      [((scimitar-typed (scimitar-for i en em eb) ty))
       (let*-values ([(tyn) (scimitar-typed-ty en)]
                     [(tym) (scimitar-typed-ty em)]
                     [(tynm) (ty-least-upper-bound tyn tym)] ;; the fallback type
                     [(tyi) (second (or (env-assoc i (scimitar-expr->env eb)) (list #f tynm)))]
                     [(f) (gensym 'f)]
                     [(j) (gensym 'j)]
                     [(xb) (gensym 'eb)]
                     [(xm) (gensym 'm)])
         (values (both-phases
                   (scimitar-typed
                     (scimitar-let `((,xm ,em))
                       (scimitar-typed
                         (scimitar-fix
                           `((,f ,(scimitar-typed
                                    (scimitar-lambda i
                                      (scimitar-typed
                                        (scimitar-let
                                          `((,j ,(scimitar-typed
                                                   (scimitar-plus
                                                     (scimitar-typed
                                                       (scimitar-num 1)
                                                       tyi)
                                                     (scimitar-typed
                                                       (scimitar-var i)
                                                       tyi))
                                                   tyi))
                                            (,xb ,eb))
                                          (scimitar-typed
                                            (scimitar-if
                                              (scimitar-typed
                                                (scimitar-lt
                                                  (scimitar-typed
                                                    (scimitar-var j)
                                                    tyi)
                                                  (scimitar-typed
                                                    (scimitar-var xm)
                                                    tyi))
                                                (bitty))
                                              (scimitar-typed
                                                (scimitar-begin
                                                  `(,(scimitar-typed
                                                       (scimitar-var xb)
                                                       ty)
                                                    ,(scimitar-typed
                                                       (scimitar-app
                                                         (scimitar-typed
                                                           (scimitar-var f)
                                                           (polyty tyi '() ty))
                                                         (scimitar-typed
                                                           (scimitar-var j)
                                                           tyi))
                                                       ty)))
                                                ty)
                                              (scimitar-typed
                                                (scimitar-var xb)
                                                ty))
                                            ty))
                                        ty))
                                    (polyty tyi '() ty))))
                           (scimitar-typed
                             (scimitar-app
                               (scimitar-typed
                                 (scimitar-var f)
                                 (polyty tyi '() ty))
                               en)
                             ty))
                         ty))
                     ty))
                 (env-empty)
                 '()))]
      [((scimitar-typed (scimitar-sum i en em eb) ty))
       (define ((uses? i) fmap p? get-x e)
         (define found (box #f))
         (fmap
           p?
           (lambda (var)
             (set-box! found (or (unbox found) (equal? (get-x var) i)))
             var)
           e)
         (unbox found))
       (define ((ir-uses? i) e)
         ((uses? i) ir-fmap ir-var? ir-var-name e))
       (define ((scimitar-uses? i) e)
         ((uses? i) scimitar-fmap scimitar-var? scimitar-var-x e))
       (define (partition-uses badvars Gamma-bad Gamma csb-bad csb eta-bad eta)
         (if (null? badvars)
           (values Gamma-bad Gamma csb-bad csb eta-bad eta)
           (let*-values ([(csb-with csb-without) (partition (ir-uses? (car badvars)) csb)]
                         [(eta-with eta-without) (partition (compose (scimitar-uses? (car badvars)) cadr) eta)]
                         [(fvs) (remove (car badvars) (concat (append (map ir-free-in csb-with) (map (compose scimitar-free-in cadr) eta-with))))]
                         [(Gamma-with) (env-filter-keys fvs Gamma)]
                         [(newbadvars) (env-dom Gamma-with)]
                         [(Gamma-without) (env-remove* fvs Gamma)])
             (partition-uses (append (cdr badvars) newbadvars)
                             (env-union-key Gamma-bad Gamma-with) Gamma-without
                             (append csb-bad csb-with) csb-without
                             (append eta-bad eta-with) eta-without))))
       (let ((tyi (second (or (env-assoc i (scimitar-expr->env eb)) (list #f (unitty))))))
         (match* (en em)
           [((scimitar-typed (scimitar-num n) _) (scimitar-typed (scimitar-num m) _))
            (let*-values ([(eb Gb csb) (go eb)]
                          ; eb is the body of the expression, in IR
                          [(eb eta) (traverse (make-irex eb))]
                          ; csb is the constraints transformed to IR
                          [(csb etas) (for/lists (csb etas) ([c csb]) (traverse c))]
                          ; eta has the values that have to be in scimitar, minus the sum variable
                          [(eta) (env-remove i (apply env-checked-union-key eta etas))]
                          ; the "withs" has the sum var, without does not
                          [(Gamma Gamma-without csb csb-without eta eta-without)
                           (partition-uses `(,i) (env-empty) Gb '() csb '() (env-entries eta))]
                          ; the tys are of the eta that uses the sum var, plus the types in the Gamma that uses the sum var
                          [(tys) (append (env-range Gamma) (map (compose scimitar-typed-ty cadr) eta))]
                          ; Gamma-omicrons makes a list for each 'i that substitutes the  vars that use with a new symbol for each 'i
                          [(Gamma-omicrons) (build-list (- m n) (lambda _ (map (lambda (x) `(,x ,(gensym x))) (env-dom Gamma))))]
                          ; likewise for eta
                          [(eta-omicrons) (build-list (- m n) (lambda _ (map (lambda (x) `(,x ,(gensym x))) (map car eta))))]
                          [(omicrons) (map append Gamma-omicrons eta-omicrons)]
                          [(ir-omicrons) (map (curry map-cadr ir-var) omicrons)]
                          [(sc-omicrons) (map (lambda (o) (map (lambda (xy ty) `(,(car xy) ,(scimitar-typed (scimitar-var (cadr xy)) ty))) o tys)) omicrons)]
                          ; Gamma substitutes each for its gensym'd var
                          [(Gamma) (env-union-key Gamma-without
                                     (env (concat (map (lambda (o) (map (lambda (e xy) `(,(cadr xy) ,(cadr e)))
                                                                        (env-entries Gamma) o))
                                                       Gamma-omicrons))))]
                          [(is) (range (- m n))]
                          ; csb does the same using the IR vars
                          [(csb) (apply append csb-without
                                   (map (lambda (j o)
                                          (let ((o (cons `(,i ,(ir-num (+ j n))) o)))
                                            (map (curry ir-subst o) csb)))
                                        is ir-omicrons))]
                          ; eta does the same using the scimitar vars
                          [(eta) (apply append eta-without
                                   (map (lambda (j o sc-o)
                                          (let ((sc-o (cons `(,i ,(scimitar-typed (scimitar-val (vec-dense (+ j n))) tyi)) sc-o)))
                                            (map (lambda (xy xe) `(,(cadr xy) ,(subst sc-o (cadr xe)))) o eta)))
                                        is eta-omicrons sc-omicrons))]
                          ; cse are the new constraints using the variables from the new eta
                          [(cse) (map (lambda (xe)
                                        (ir-constraint (ir-var (car xe)) '= (make-irex (phase2-bothex (cadr xe) (ir-var (car xe))))))
                                      eta)])
              (values (ir-typed
                        (if (andmap null? ir-omicrons)
                          (ir-vec-sum i (ir-num n) (ir-num m) eb)
                          (foldl1
                            ir-add
                            (build-list (- m n)
                              (lambda (j)
                                (ir-subst (list-ref ir-omicrons j) eb)))))
                        ty
                        (inside-solve?))
                      Gamma
                      (append csb cse)))]
           [((scimitar-typed _ tyn) (scimitar-typed _ tym))
            (let*-values ([(f) (gensym 'f)]
                          [(j) (gensym 'j)]
                          [(acc) (gensym 'acc)]
                          [(xm) (gensym 'm)])
              (values (both-phases
                        (scimitar-typed
                          (scimitar-let `((,xm ,em))
                            (scimitar-typed
                              (scimitar-fix
                                `((,f ,(scimitar-typed
                                         (scimitar-lambda `(,i ,acc)
                                           (scimitar-typed
                                             (scimitar-let*
                                               `((,j ,(scimitar-typed
                                                        (scimitar-plus
                                                          (scimitar-typed
                                                            (scimitar-num 1)
                                                            tyi)
                                                          (scimitar-typed
                                                            (scimitar-var i)
                                                            tyi))
                                                        tyi))
                                                 (,acc ,(scimitar-typed
                                                          (scimitar-plus
                                                            (scimitar-typed
                                                              (scimitar-var acc)
                                                              ty)
                                                            eb)
                                                          ty)))
                                               (scimitar-typed
                                                 (scimitar-if
                                                   (scimitar-typed
                                                     (scimitar-lt
                                                       (scimitar-typed
                                                         (scimitar-var j)
                                                         tyi)
                                                       (scimitar-typed
                                                         (scimitar-var xm)
                                                         tyi))
                                                     (bitty))
                                                   (scimitar-typed
                                                     (scimitar-app
                                                       (scimitar-typed
                                                         (scimitar-var f)
                                                         (polyty (tuplety tyi ty) '() ty))
                                                       (scimitar-typed
                                                         (scimitar-tuple
                                                           `(,(scimitar-typed
                                                                (scimitar-var j)
                                                                tyi)
                                                             ,(scimitar-typed
                                                                (scimitar-var acc)
                                                                ty)))
                                                         (tuplety tyi ty)))
                                                     ty)
                                                   (scimitar-typed
                                                     (scimitar-var acc)
                                                     ty))
                                                 ty))
                                             ty))
                                         (polyty (tuplety tyi ty) '() ty))))
                                (scimitar-typed
                                  (scimitar-app
                                    (scimitar-typed
                                      (scimitar-var f)
                                      (polyty (tuplety tyi ty) '() ty))
                                    (scimitar-typed
                                      (scimitar-tuple
                                        `(,en
                                          ,(scimitar-typed
                                             (scimitar-num 0)
                                             ty)))
                                      (tuplety tyi ty)))
                                  ty))
                              ty))
                          ty))
                      (env-empty)
                      '()))]))]
      [((scimitar-typed (scimitar-constraint lhs ineq rhs) ty))
       (let-values ([(lhs Gl lcs) (go lhs)]
                    [(rhs Gr rcs) (go rhs)])
         (values (ir-typed (ir-unit) ty (inside-solve?))
                 (env-checked-union-key Gl Gr)
                 (append `(,(ir-constraint (make-irex lhs) ineq (make-irex rhs))) lcs rcs)))]
      [((scimitar-typed (scimitar-infeasible) ty))
       (values e (env-empty) '())]
      [((scimitar-typed (scimitar-symbolic) ty))
       (define-ir
         (op-floating () (: r ty)))
       (values (ir-typed (ir-poly-app
                           (ir-poly op-floating)
                           (ir-unit))
                         ty (inside-solve?))
               (env-empty) '())]
      [((scimitar-typed (and e (scimitar-typed _ ty1)) ty2))
       (let-values ([(e G cs) (go e)])
         (values
           (match e
             [(phase2-bothex scexpr irexpr)
              (phase2-bothex (scimitar-typed scexpr ty2) (ir-cast irexpr ty2))]
             [(? scimitar-expr?) (scimitar-typed e ty2)]
             [(? ir-expr?) (ir-typed (ir-cast e ty2) ty2 (inside-solve?))])
           G
           cs))]
      [((scimitar-typed e ty))
       (error "got to a bad place")])
    (define/match (traverse e)
      [((ir-constraint lhs ineq rhs))
       (let-values ([(lhs etal) (traverse lhs)]
                    [(rhs etar) (traverse rhs)])
         (values
           (ir-constraint lhs ineq rhs)
           (env-checked-union-key etal etar)))]
      [((or (ir-typed (and e (or (ir-add e1 e2) (ir-scmul e1 e2) (ir-var-mul e1 e2) (ir-vec-ix e1 e2))) _ _)
            (and e (or (ir-add e1 e2) (ir-scmul e1 e2) (ir-var-mul e1 e2) (ir-vec-ix e1 e2)))))
       (let-values ([(op) (match e
                            [(? ir-add?) ir-add]
                            [(? ir-scmul?) ir-scmul]
                            [(? ir-var-mul?) ir-var-mul]
                            [(? ir-vec-ix?) ir-vec-ix])]
                    [(e1 eta1) (traverse e1)]
                    [(e2 eta2) (traverse e2)])
         (values (op e1 e2) (env-checked-union-key eta1 eta2)))]
      [((or (ir-typed (ir-num n) _ _) (ir-num n)))
       (values (ir-num n) (env-empty scimitar-expr?))]
      [((or (ir-typed (ir-vec v) _ _) (ir-vec v)))
       (values (ir-vec v) (env-empty scimitar-expr?))]
      [((or (ir-typed (ir-unit) _ _) (ir-unit)))
       (values (ir-unit) (env-empty scimitar-expr?))]
      [((ir-poly _))
       (values e (env-empty scimitar-expr?))]
      [((or (ir-typed (? ir-var? e) _ _)
            (? ir-var? e)))
       (values e (env-empty scimitar-expr?))]
      [((or (ir-typed (ir-vec-sum i s e t) _ _)
            (ir-vec-sum i s e t)))
       ;; On the surface, this seems incorrect, but:
       ;; 1) s and e are nums and don't need to be traversed
       ;; 2) (t) has already been traversed in go
       (values (ir-vec-sum i s e t) (env-empty scimitar-expr?))]
      [((or (phase2-irex (phase2-bothex _ (ir-cast e ty)))
            (ir-typed (ir-cast e ty) _ _)
            (ir-cast e ty)))
       (let-values ([(e eta) (traverse e)])
         (values (ir-cast e ty) eta))]
      [((or (phase2-irex (phase2-bothex _ (ir-tuple es)))
            (ir-tuple es)))
       (let-values ([(es etas) (for/lists (es etas) ([e es]) (traverse e))])
         (values (ir-tuple es) (apply env-checked-union-key etas)))]
      [((phase2-irex (phase2-bothex e (ir-poly-app (phase2-irex f) args))))
       #:when (or (not (phase2-bothex? f)) (ir-var? (phase2-bothex-irexpr f)))
       ;; This case is special because it is either a variable, which
       ;; ir-poly-app cannot handle, or a lambda, which gets converted
       ;; to a variable later in the catch-all scimitar-expr case below
       (traverse (phase2-irex e))]
      [((ir-poly-app (phase2-irex f) args))
       #:when (or (not (phase2-bothex? f)) (ir-var? (phase2-bothex-irexpr f)))
       ;; This case is similar to the previous
       (when (not (phase2-irex? args))
         (error "During desugaring, can't handle non-poly function with ir arguments"))
       (let ((app (scimitar-app (make-scex                      f  (env-empty) '())
                                (make-scex (phase2-irex-expr args) (env-empty) '())))
             (ty (polyty-cod
                   (scimitar-typed-ty
                     (if (not (phase2-bothex? f)) f (phase2-bothex-scexpr f))))))
         (traverse (phase2-irex (scimitar-typed app ty))))]
      [((or (phase2-irex (phase2-bothex _ (ir-poly-app f args)))
            (ir-typed (ir-poly-app f args) _ _)
            (ir-poly-app f args)))
       (let-values ([(f etaf) (traverse f)]
                    [(args etaa) (traverse args)])
         (values (ir-poly-app f args) (env-checked-union-key etaf etaa)))]
      [((phase2-irex (phase2-bothex e (ir-var x))))
       (let ((e (compact-ir e)))
         (values (ir-var x) (env `((,x ,e)) scimitar-expr?)))]
      [((phase2-irex (phase2-bothex _ e)))
       (values e (env-empty scimitar-expr?))]
      [((phase2-irex (? scimitar-expr? e)))
       (let ((e (compact-ir e))
             (x (gensym 'x)))
         (values (ir-var x) (env `((,x ,e)) scimitar-expr?)))])
    (define (compact-ir e)
      (define/match (wrap e)
        [((phase2-scex (phase2-bothex e _) _ _))
         (compact-ir e)]
        [((phase2-scex (ir-typed e ty inside-solve) Gamma-l cs))
         (let*-values ([(e eta) (traverse e)]
                       [(cs etas) (for/lists (cs etas) ([c cs]) (traverse c))]
                       [(eta) (apply env-checked-union-key eta etas)]
                       [(Gamma) (env-map-vals scimitar-typed-ty eta ty?)]
                       [(r) (gensym 'r)]
                       [(Gamma-i) (if inside-solve Gamma (env-add r ty Gamma))]
                       [(Gamma-r) (if inside-solve (env `((,r ,ty))) (env-empty))]
                       [(p) (ir-compile
                              (ir-poly-dec
                                (gensym 'embedded-)
                                Gamma-i
                                Gamma-r
                                Gamma-l
                                (cons
                                  (ir-constraint (ir-var r) '= e)
                                  cs))
                              (env-empty)
                              (env-empty number?)
                              (env-empty poly?))])
           (scimitar-typed
             (scimitar-app
               (if inside-solve
                 (scimitar-typed (scimitar-poly p) (poly-ty p))
                 (scimitar-typed
                   (scimitar-lambda (sym-val-list->sym-val (env-dom Gamma))
                     (scimitar-typed
                       (scimitar-solve 'minimize r (scimitar-num 0) `(,r)
                         `(,(scimitar-typed
                              (scimitar-app
                                (scimitar-typed (scimitar-poly p) (poly-ty p))
                                (scimitar-typed-list->scimitar-typed
                                  (map (lambda (x ty)
                                         (scimitar-typed (scimitar-var x) ty))
                                       (env-dom Gamma-i)
                                       (env-range Gamma-i))))
                              (polyty-cod (poly-ty p)))))
                       ty))
                   (polyty (env->ty Gamma) '() ty)))
               (scimitar-typed-list->scimitar-typed (env-range eta)))
             ty))])
      (scimitar-fmap phase2-scex? wrap e))
    (let-values ([(e G cs) (go e)])
      (when (not (env-empty? G))
        (error (format "During desugar, there shouldn't be any new top level variables: ~v" (env-dom G))))
      (compact-ir (make-scex e G cs))))
  (define (both-phases e)
    (phase2 (phase1 e)))
  (both-phases e))
