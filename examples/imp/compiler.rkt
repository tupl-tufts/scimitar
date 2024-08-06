#lang racket

(require scimitar/contract-utils)
(require "grammar.rkt")
(require "elab.rkt")
(require "semantics/ba.ir.rkt")
(require "semantics/bv.ir.rkt")
(require "semantics/env.ir.ast.rkt")
(require scimitar/scimitar/grammar)
(require scimitar/scimitar/util)
(require (only-in scimitar/env
           env-range env? env->ty))
(require scimitar/params)
(require scimitar/ty)
(require scimitar/util)
(require scimitar/val)

(provide imp-compile)


(define/contract (imp-expr-compile in e Gamma)
  (-> symbol? imp-expr? env? scimitar-expr?)
  (define-syntax-rule (binop p e1 e2)
    (scimitar-app (scimitar-poly p)
      (scimitar-tuple `(,(go e1) ,(go e2)))))
  (define-syntax-rule (unop p q)
    (scimitar-app (scimitar-poly p) (go q)))
  (define-syntax-rule (fun p)
    (scimitar-app (scimitar-poly p) (scimitar-var in)))
  (define/match (go e)
    [((imp-plus e1 e2)) (binop (bv-add) e1 e2)]
    [((imp-minus e1 e2)) (binop (bv-sub) e1 e2)]
    [((imp-le e1 e2)) (binop (bv-le) e1 e2)]
    [((imp-eq e1 e2)) (scimitar-eq (go e1) (go e2))]
    [((imp-bool v)) (scimitar-val (boolean->bit v))]
    [((imp-not q))  (unop ba-not q)]
    [((imp-and q1 q2)) (binop ba-and q1 q2)]
    [((imp-or q1 q2)) (binop ba-or q1 q2)]
    [((imp-nat n)) (scimitar-val (int->bv n))]
    [((imp-var x)) (fun (env-var x Gamma))])
  (go e))

(struct op () #:transparent)
(struct op-scim op (expr) #:transparent)
(struct op-label op (name) #:transparent)
(struct op-cond op (expr lfalse) #:transparent)
(struct op-jmp op (name) #:transparent)
(define/contract (imp-stmt->ops in s Gamma)
  (-> symbol? imp-stmt? env? (listof op?))
  (define/match (go s)
    [((imp-skip)) '()]
    [((imp-assign x e))
     `(,(op-scim
          (scimitar-app (scimitar-poly (env-set x Gamma))
            (scimitar-tuple `(,(imp-expr-compile in e Gamma) ,(scimitar-var in))))))]
    [((imp-if q st sf))
     (let ((l-false (gensym 'false))
           (l-out (gensym 'out)))
       `(,(op-cond (imp-expr-compile in q Gamma) l-false)
         ,@(go st)
         ,(op-jmp l-out)
         ,(op-label l-false)
         ,@(go sf)
         ,(op-label l-out)))]
    [((imp-while q bs))
     (let ((l-while (gensym 'while))
           (l-out (gensym 'out)))
       `(,(op-label l-while)
         ,(op-cond (imp-expr-compile in q Gamma) l-out)
         ,@(flatten (map go bs))
         ,(op-jmp l-while)
         ,(op-label l-out)))])
  (go s))

(define/contract (ops-compile in ops)
  (-> symbol? (listof op?) scimitar-expr?)
  (define (ops->blocks ops)
    (define/match (to-blocks ops)
      [('()) '(())]
      [(`(,op . ,ops))
       (match (to-blocks ops)
         [`(,b . ,bs)
          (let ((bs `((,op . ,b) . ,bs)))
            (if (op-label? op) (cons '() bs) bs))])])
    (define (add-jump b f)
      (if (and (pair? b) (op-jmp? (last b)))
        b
        (snoc b (op-jmp f))))
    (define/match (add-jumps bs)
      [(`(,_)) bs]
      [(`(,b (,(op-label f) . ,_) . ,_))
       (cons (add-jump b f) (add-jumps (cdr bs)))])
    (add-jumps (to-blocks ops)))
  (match-define `(,start . ,blocks)
    (ops->blocks ops))
  (define/match ((cut-jump f) b)
    [(f `(,@ops ,(op-jmp h))) #:when (equal? f h) ops]
    [(_ b) b])
  (define/match (cut-binding f bs)
    [(f `(,@bs1 (,(op-label h) ,@_) ,@bs2))
     #:when (equal? f h) (append bs1 bs2)]
    [(_ _) bs])
  (define (jump-dsts bs)
    (map op-jmp-name (filter op-jmp? (flatten bs))))
  (define (cond-jump-dsts bs)
    (map op-cond-lfalse (filter op-cond? (flatten bs))))
  (define (all-jump-dsts bs)
    (nub (append (jump-dsts bs) (cond-jump-dsts bs))))
  (define (all-labels bs)
    (map (compose op-label-name first) bs))
  (define nop? (compose null? cdr))
  (define (get-nops bs)
    (map (curry list 'nop)
         (all-labels (filter nop? bs))))
  (define (get-calls b)
    (map (curry list 'called)
         (all-jump-dsts `(,b))))
  (match-define `(,intro ,bindings ,_)
    (work (match-lambda**
            [(`(called ,f) (and acc `(,start ,used ,bs)))
             (match* ((member f (all-jump-dsts `(,start ,@used)))
                      (assoc (op-label f) used)
                      (assoc (op-label f) bs))
               [(#f  _  _) (values '() acc)]
               [( _ #f #f) (error "BUG in work")]
               [( _  u #f) (values '() acc)]
               [( _ #f  b) (values (get-calls b) `(,start (,b ,@used) ,(cut-binding f bs)))]
               [( _  _  _) (error "BUG in work")])]
            [(`(nop ,f) `(,start ,used ,bs))
             (let* ((cut-jump-f (cut-jump f))
                    (s (cut-jump-f start)))
               (if (null? (all-jump-dsts `(,s)))
                 (values '() `(,s () ()))
                 (let* ((used (map cut-jump-f used))
                        (bs (map cut-jump-f bs))
                        (present (or (member f (cond-jump-dsts used))
                                     (member f (cond-jump-dsts bs))))
                        (used (if present used (cut-binding f used)))
                        (bs (if present bs (cut-binding f bs)))
                        (q (nub (append (get-nops used) (get-nops bs))))
                        (q (if present (filter-cadr (lambda (g) (not (equal? f g))) q) q)))
                   (values q `(,s ,used ,bs)))))])
          (append (get-nops blocks) (get-calls start))
          `(,start () ,blocks)))
  (define (make-solve e)
    (let ((out (gensym 'result)))
      (scimitar-solve 'minimize out (scimitar-var out) `(,out)
        `(,(scimitar-constraint (scimitar-var out) '= e)))))
  (define/match (make-solves es)
    [(`(,e)) (make-solve e)]
    [(`(,@es ,e))
     (make-solve
       (scimitar-let*
         (map (curry list in) es)
         e))])
  (define/match (exprs->solve es p)
    [('() #f) (scimitar-var in)]
    [('() p) p]
    [(es #f) (make-solves es)]
    [(es (scimitar-app f (scimitar-var x))) #:when (equal? x in) (scimitar-app f (make-solves es))]
    [(es p) (scimitar-let `((,in ,(make-solves es))) p)])
  (define/match (op->scimitar op p)
    [((op-scim e) `(,es ,p)) `((,e ,@es) ,p)]
    [((op-cond e f) `(,es ,p))
     `(() ,(scimitar-if (make-solve e)
             (exprs->solve es p)
             (if (member `(nop ,f) (get-nops bindings))
               (scimitar-var in)
               (scimitar-app (scimitar-var f) (scimitar-var in)))))]
    [((op-jmp f) _) `(() ,(scimitar-app (scimitar-var f) (scimitar-var in)))]
    [((op-label _) _) (error "BUG in Imp op->scimitar")])
  (define (ops->scimitar ops)
    (apply exprs->solve (foldr op->scimitar `(() #f) ops)))
  (define/match (block-binding->scimitar-binding binding)
    [(`(,(op-label f) ,@ops))
     `(,f ,(scimitar-lambda in (ops->scimitar ops)))])
  (let* ((nops (all-labels (filter nop? bindings)))
         (bindings (foldr cut-binding bindings nops)))
    (if (null? bindings)
      (ops->scimitar intro)
      (scimitar-fix
        (map block-binding->scimitar-binding bindings)
        (ops->scimitar intro)))))

(define (default-val ty)
  (cond
    [(equal? ty boolty)
     (boolean->bit #f)]
    [(equal? ty naturalty)
     (int->bv 0)]))

(define/contract (imp-compile p Gamma)
  (-> (listof imp-stmt?) env? scimitar-expr?)
  (let* ((args (scimitar-val (sym-val-list->sym-val (map default-val (env-range Gamma)))))
         (in (gensym 'in)))
    (scimitar-let `((,in ,args))
      (ops-compile in
        (flatten (map (curryr (curry imp-stmt->ops in) Gamma) p))))))
