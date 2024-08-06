#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "error.rkt")
(require "grammar.rkt")
(require "ty.rkt")
(require "util.rkt")

(provide
  ir-dec-linearize)

;; Fix the dimensionality of the problem by rewriting into a SSA form.
;; The variables altogether make up the dimensionality EXCEPT polys.
(define/contract (ir-expr-linearize e Gamma [spill #f] [refuse-to-spill #f])
  (->* (ir-expr? env?) (boolean? boolean?) (values env? (listof (list/c ir-var? ir-expr?)) ir-expr?))
  (define (ir-expr-list-linearize es)
    (apply values
      (foldr (match-lambda*
               [`(,e (,Gamma_es ,css ,es))
                (let-values (((Gamma_e cs e) (ir-expr-linearize e Gamma)))
                  (list (env-union-key Gamma_e Gamma_es) (append cs css) (cons e es)))])
             `(,(env-empty) () ()) es)))
  (match e
    [(? ir-poly?)
     (let* ((ty (ir-val-check e))
            (x (gensym 'poly-))
            (vx (ir-var x)))
       (values (env `((,x ,ty))) `((,vx ,e)) vx))]
    [(? ir-val?)
     (if refuse-to-spill
       (values (env-empty) '() e)
       (let* ((ty (ir-val-check e))
              (x (gensym 'val-))
              (vx (ir-var x)))
         (values (env `((,x ,ty))) `((,vx ,e)) vx)))]
    [(ir-var _) (values (env-empty) '() e)]
    [(ir-add e1 e2)     ;; returns (+ (.* n x) (.* m y) ...)
     (let*-values ([(Gamma1 cs1 e1) (ir-expr-linearize e1 Gamma)]
                   [(Gamma2 cs2 e2) (ir-expr-linearize e2 Gamma)]
                   [(Gamma_e) (env-union-key Gamma1 Gamma2)])
       (if spill
         (let* ((ty (ir-expr-check e Gamma))
                (x (gensym 'add-))
                (vx (ir-var x)))
           (values (env-add x ty Gamma_e) (append `((,vx ,(ir-add e1 e2))) cs1 cs2) vx))
         (values Gamma_e (append '() cs1 cs2) (ir-add e1 e2))))]
    [(ir-scmul m ex)    ;; returns (.* n x)
     (let-values ([(Gamma_e cs ex) (ir-expr-linearize ex Gamma)])
       (if spill
         (let* ((ty (ir-expr-check e Gamma))
                (x (gensym 'scmul-))
                (vx (ir-var x)))
           (values (env-add x ty Gamma_e) (append `((,vx ,(ir-scmul m ex))) cs) vx))
         (values Gamma_e (append '() cs) (ir-scmul m ex))))]
    [(ir-var-mul e1 e2)
     (let*-values ([(Gamma1 cs1 e1) (ir-expr-linearize e1 Gamma #t)]
                   [(Gamma2 cs2 e2) (ir-expr-linearize e2 Gamma #t)]
                   [(Gamma_e) (env-union-key Gamma1 Gamma2)])
       (if refuse-to-spill
         (values Gamma_e (append cs1 cs2) (ir-var-mul e1 e2))
         (let* ((ty (ir-expr-check e Gamma))
                (x (gensym 'var-mul-))
                (vx (ir-var x)))
           (values (env-add x ty Gamma_e) (cons `(,vx ,(ir-var-mul e1 e2)) (append cs1 cs2)) vx))))]
    [(ir-tuple es)
     (let*-values ([(vs) (ir-val->val e)]
                   [(Gamma_e ces es) (if vs (values (env-empty) '() '()) (ir-expr-list-linearize es))]
                   ;; as an optimization, gen different code for all vals
                   [(e2) (if vs (ir-vec (val->vec vs)) (ir-tuple es))])
       (if refuse-to-spill
         (values Gamma_e ces e2)
         (let* ((ty (ir-expr-check e Gamma))
                (x (gensym 'tuple-))
                (vx (ir-var x)))
           (values (env-add x ty Gamma_e)  (cons `(,vx ,e2) ces) vx))))]
    [(ir-vec-ix v ix)
     (let*-values ([(Gamma_v cs v)     (ir-expr-linearize v Gamma #t)]
                   [(Gamma_ix csin ix) (if (ir-ix? ix)
                                         ;; leave in measurement as an optimization
                                         (values (env-empty) '() ix)
                                         (ir-expr-linearize ix Gamma #t (ir-tuple? ix)))]
                   [(Gamma_e) (env-union-key Gamma_v Gamma_ix)])
       (if refuse-to-spill
         (values Gamma_e (append cs csin) (ir-vec-ix v ix))
         (let* ((ty (ir-expr-check e Gamma))
                (x (gensym 'vec-ix-))
                (vx (ir-var x)))
           (values (env-add x ty Gamma_e) (cons `(,vx ,(ir-vec-ix v ix)) (append cs csin)) vx))))]
    [(ir-cast e ty)
     (let-values ([(Gamma_e cs e) (ir-expr-linearize e Gamma #t)])
       (cond
         [refuse-to-spill
          (values Gamma_e cs (ir-cast e ty)) ]
         [(equal? ty (ir-expr-check e (env-union-key Gamma Gamma_e)))
          (values Gamma_e cs e)]
         [else
          (let* ((x (gensym 'cast-ix-))
                 (vx (ir-var x)))
            (values (env-add x ty Gamma_e) (cons `(,vx ,(ir-cast e ty)) cs) vx))]))]
    [(ir-poly-app f xs)
     (let*-values ([(Gamma_f  csf f)  (ir-expr-linearize f Gamma)]
                   [(vs) (ir-val->val xs)]
                   [(Gamma_xs cxs xs) (if vs (values (env-empty) '() (ir-vec (val->vec vs))) (ir-expr-linearize xs Gamma #f #t))]
                   [(Gamma_e) (env-union-key Gamma_f Gamma_xs)])
       (if spill
         (let* ((ty (ir-expr-check e Gamma))
                (x (gensym 'poly-app-))
                (vx (ir-var x)))
           (values (env-add x ty Gamma_e) (cons `(,vx ,(ir-poly-app f xs)) (append csf cxs)) vx))
         (values Gamma_e (append csf cxs) (ir-poly-app f xs))))]
    [else
      (ir-unexpected "linearize" e)]))

(define/contract (ir-stmt-linearize s Gamma)
  (-> ir-stmt? env? (list/c env? (listof ir-stmt?)))
  (define ir-assignable?
    (or/c ir-val? ir-vec-ix? ir-var-mul?))
  (define (do-sub e1 e2)
    (match e2
      [(ir-scmul (ir-num -1) e2) (ir-add e1 e2)]
      [_ (ir-sub e1 e2)]))
  (define (format-constraint c)
    (match c
      [`(,x ,(? ir-assignable? e)) (ir-constraint x '= e)]
      [`(,x ,e) (ir-constraint (do-sub x e) '= (ir-num 0))]))
  (match s
    ;; special case optimizations for direct assignment
    [(or (ir-constraint (ir-add (? ir-var? e1) (ir-scmul (ir-num -1) e2)) '= (ir-num 0))
         (ir-constraint (ir-add (ir-scmul (ir-num -1) e2) (? ir-var? e1)) '= (ir-num 0))
         (ir-constraint (ir-add e2 (ir-scmul (ir-num -1) (? ir-var? e1))) '= (ir-num 0))
         (ir-constraint (ir-add (ir-scmul (ir-num -1) (? ir-var? e1)) e2) '= (ir-num 0)))
     (let-values ([(Gamma_e cs e2) (ir-expr-linearize e2 Gamma #f #t)])
       `(,Gamma_e
         ,(cons (format-constraint `(,e1 ,e2))
                (map format-constraint cs))))]
    [(ir-constraint lhs ineq rhs)
     (let-values ([(Gamma_e cs lhs) (ir-expr-linearize lhs Gamma)])
       `(,Gamma_e
         ,(cons (ir-constraint lhs ineq rhs)
                (map format-constraint cs))))]
    [else
      (ir-unexpected "linearize" s)]))

(define/contract (ir-dec-linearize d Gamma)
  (-> ir-dec? env? ir-dec?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (match-let* ([Gamma (env-union-key Gamma_l Gamma_r Gamma_f Gamma)]
                  [`(,Gammas ,sss) (if (null? ss) `((,(env)) ())
                                     (unzip (map (curryr ir-stmt-linearize Gamma) ss)))])
       (ir-poly-dec f Gamma_f Gamma_r (apply env-union-key Gamma_l Gammas) (concat sss)))]
    [else
      (ir-unexpected "linearize" d)]))
