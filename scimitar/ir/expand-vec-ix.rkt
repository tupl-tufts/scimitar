#lang racket

(require "../contract-utils.rkt")
(require "error.rkt")
(require "grammar.rkt")
(require "util.rkt")
(require "../env.rkt")
(require "../topo.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")

(provide
  ir-dec-expand-vec-ix)

(define/contract (ir-stmt-expand-vec-ix s Gamma)
  (-> ir-stmt? env? (list/c env? (listof ir-stmt?)))
  (match s
    [(ir-constraint (? ir-var? x) '= (and (ir-vec-ix _ (? ir-ix?)) e))
     ;; Leave this case for lowering since it's radically different
     `(,(env-empty) (,(ir-constraint (ir-sub x e) '= (ir-num 0))))]
    [(ir-constraint (ir-var x) '= (ir-vec-ix v ix))
     (let* ((Gixs (if (ir-var? ix)
                    (map (lambda (t) `(,(gensym 'ix-var-) ,t))
                           (ty->ty-list (second (env-assoc (ir-var-name ix) Gamma))))
                    '()))
            (ixt (if (ir-var? ix)
                   (sym-val->ir-val (sym-val-list->sym-val (map car Gixs)))
                   ix))
            (ixs (map (match-lambda
                       [(ir-var x) x]
                       [(ir-num n) n]
                       [(ir-vec v)
                        (match (vec-flatten v)
                          [(and (? (vec-shape/c (equal/c '(1)))) (vec-dense `(,n))) n]
                          [_ (error (format "Vector of shape ~v not supported in vector indexing" (vec-shape v)))])])
                     (ir-val->ir-val-list ixt)))
            (s (vecty-shape (second (env-assoc (ir-var-name v) Gamma))))
            (coords (filter (lambda (coord)
                              (andmap (lambda (i c) (or (symbol? i) (= c i))) ixs coord))
                            (measurement-gen-coords s)))
            (ixs-rho
              (foldr (match-lambda*
                       [`(,i (,ixs ,rho))
                        (if (number? i)
                          (let ((n (gensym 'n)))
                            `(,(cons n ixs) ,(cons `(,n ,i) rho)))
                          `(,(cons i ixs) ,rho))])
                     '(() ()) ixs))
            (ixs (car ixs-rho))
            (rho (cadr ixs-rho))
            (bounds (init (scanl * 1 s)))
            (scms (foldr (lambda (b x scms) (cons (ir-scmul (ir-num b) (ir-var x)) scms)) '() bounds ixs))
            (en (foldr1 ir-add scms))
            ;; an indicator variable for each possible index
            (indxs (map (lambda (_) (gensym 'ix-ind-)) coords))
            (indvs (map ir-var indxs))
            ;; the sum of all indicator variables is one
            (exclsv (foldr1 ir-add indvs))
            (one (gensym 'one))
            ;; the sum of each indicator times its corresponding coordinate equals the target index y
            (select (foldr1 ir-add (map (lambda (i c) (ir-scmul (ir-num (measurement-scaled-L1-norm c bounds)) i)) indvs coords)))
            ;; the concrete indices of v
            (vixs (map (lambda (c) (ir-vec-ix v (ir-val-list->ir-val (map ir-num c)))) coords))
            (vixxs (map (lambda (_) (gensym 'ix-vec-ix-)) vixs))
            (vixvs (map ir-var vixxs))
            ;; the products of the indicator variables and the concrete indices of v
            (vmuls (map ir-var-mul indvs vixvs))
            (vmulxs (map (lambda (_) (gensym 'ix-var-mul-)) vmuls))
            (vmulvs (map ir-var vmulxs))
            ;; the output of ref is the sum of these products
            (coefs (foldr1 ir-add vmulvs))
            (tyvix (second (env-assoc x Gamma))))
       `(,(env (append Gixs
                       `((,one ,(vecty (interval 1 1) '())))
                       (map (lambda (xn) `(,(car xn) ,(vecty (interval (cadr xn) (cadr xn)) '()))) rho)
                       (map (lambda (x) `(,x ,(bitty))) indxs)
                       (map (lambda (x) `(,x ,tyvix)) vixxs)
                       (map (lambda (x) `(,x ,tyvix)) vmulxs)))
         (,@(if (null? Gixs) '() `(,(ir-constraint (ir-sub ix ixt) '= (ir-num 0))))
          ,(ir-constraint (ir-var one) '= (ir-num 1))
          ,(ir-constraint (ir-sub exclsv (ir-var one)) '= (ir-num 0))
          ,@(map (lambda (xn) (ir-constraint (ir-var (car xn)) '= (ir-num (cadr xn)))) rho)
          ,(ir-constraint (ir-sub select en) '= (ir-num 0))
          ,@(map (lambda (v ix)  (ir-constraint (ir-sub v ix) '= (ir-num 0))) vixvs vixs)
          ,@(map (lambda (v mul) (ir-constraint v '= mul)) vmulvs vmuls)
          ,(ir-constraint (ir-sub coefs (ir-var x)) '= (ir-num 0)))))]
    [(? ir-constraint?) `(,(env-empty) (,s))]
    [else
      (ir-unexpected "expand vec ix" s)]))

(define/contract (ir-dec-expand-vec-ix d)
  (-> ir-dec? ir-dec?)
  (match d
    [(ir-poly-dec f Gamma_f Gamma_r Gamma_l ss)
     (let* ((Gamma (env-union-key Gamma_r Gamma_f Gamma_l))
            (Gammas-sss (if (null? ss) `((,(env)) ())
                          (unzip (map (curryr ir-stmt-expand-vec-ix Gamma) ss))))
            (Gammas (car Gammas-sss))
            (sss (cadr Gammas-sss)))
       (ir-poly-dec f Gamma_f Gamma_r (apply env-union-key Gamma_l Gammas) (concat sss)))]
    [else
      (ir-unexpected "expand vec ix" d)]))
