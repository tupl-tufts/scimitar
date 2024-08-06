#lang racket

(require "contract-utils.rkt")
(require (only-in "params.rkt" debug preprocess))
(require (rename-in "solver.rkt"
           [solve do-solve]))
(require "env.rkt")
(require "poly.rkt")
(require "ty.rkt")
(require "util.rkt")
(require "val.rkt")
(require "vec.rkt")

(provide
  load-lp invoke-solver solve
  (struct-out lp) lp-default)

(struct/contract
  lp ([direction optimization_direction?]
      [objective vec?]
      [poly poly?]) #:transparent)

(define (lp-default poly)
  (let* ((obj (vec-augment 0
                (vec-1 `(,(poly-output-dim poly)))
                (vec-negate (vec-1 `(,(poly-input-dim poly))))
                (vec-0 `(,(max 0 (poly-body-width poly)))))))
    (lp 'minimize obj poly)))

(define/match (convert-vec-to-indexed-values vs)
  [((vec-sparse es))
   (let* ((es (group-by cadar es))
          (col-ixs-vs
           (foldr (match-lambda**
                    [(es `(,n (,rows ,vs)))
                     (let* ((col (cadaar es))
                            (empties (replicate (- n col) '()))
                            (new-rows (map caar es))
                            (new-vs (map cadr es)))
                       `(,(- col 1) (,(cons new-rows (append empties rows))
                                     ,(cons new-vs   (append empties   vs)))))])
                  `(,(- (vec-width vs) 1) (() ())) es))
          (col (car col-ixs-vs))
          (empties (replicate (+ 1 col) '()))
          (ixs-vs (cadr col-ixs-vs)))
     (map (curry append empties) ixs-vs))])

(define/contract (load-lp model p)
  (-> Model? lp? void?)
  (let* ((dir (lp-direction p))
         (obj (lp-objective p))
         (tys (poly-column-types (lp-poly p)))
         (cs  (poly-cs (lp-poly p)))
         (vs  (constraint-A cs))
         (ixs-vs (convert-vec-to-indexed-values vs))
         (ixs  (first  ixs-vs)) ; a list of lists of rows; some are empty
         (vals (second ixs-vs)) ; a list of lists of values; some are empty
         (collb (map (compose interval-inf vecty-interval) tys))
         (colub (map (compose interval-sup vecty-interval) tys))
         (obj (vec-unpack obj))
         (senses (constraint-ineq cs))
         (rhss (vec-unpack (constraint-b cs))))
    (loadProblem model ixs vals collb colub obj senses rhss)
    (enumerate-with
      (lambda (j ty)
        (when (and (vecty? ty) (intset? (vecty-interval ty)))
          (setInteger model j)))
      tys)
    (setObjSense model dir)))

(define/contract (invoke-solver p)
  (-> lp? (or/c vec? #f))
  (let* ((model (newModel)))
    (setLogLevel model 'none)
    (when (not preprocess)
      (setPreprocess model 'off))
    (load-lp model p)
    (do-solve model)
    (if (isProvenInfeasible model)
      (begin
        (deleteModel model)
        #f)
      (let ((result (getColSolution model)))
        (deleteModel model)
        (vec-dense result)))))

(define/contract (solve p #:which-columns [which-columns 'o])
  (->i ([p (or/c poly? lp?)])
       (#:which-columns [which-columns (or/c 'i 'o 'io 'all)])
       [result (or/c val? #f)])
  (define lp (if (poly? p) (lp-default p) p))
  (define optimum (invoke-solver lp))
  (when (debug)
    (display "Full solution:\n" (current-error-port))
    (let ((Gamma (poly-Gamma-col (lp-poly lp))))
      (if (or (not optimum) (null? (env-dom Gamma)))
        (println optimum (current-error-port))
        (for ([x (env-dom Gamma)]
              [o (vec->val (env->ty Gamma) optimum)])
          (display (format "    ~v ~v\n" x o) (current-error-port))))))
  (and optimum
       (case which-columns
         [(i) (let* ((p (lp-poly lp)) (ty (poly-ty p)))
                (vec->val (polyty-dom ty)
                          (vec-project optimum `(,(poly-output-dim p)) `(,(+ (poly-output-dim p) (poly-input-dim p))))))]
         [(o) (let* ((p (lp-poly lp)) (ty (poly-ty p)))
                (vec->val (polyty-cod ty)
                          (vec-project optimum '(0) `(,(poly-output-dim p)))))]
         [(io) (let* ((p (lp-poly lp)) (ty (poly-ty p)))
                 (vec->val (tuplety (polyty-cod ty) (polyty-dom ty))
                           (vec-project optimum '(0) `(,(+ (poly-output-dim p) (poly-input-dim p))))))]
         [(all) (vec->val (ty-list->ty (poly-all-types (lp-poly lp))) optimum)])))
(module+ test
  (require rackunit)
  (check-equal?
    (invoke-solver
      (lp 'maximize (vec-dense '(1 2))
          (poly (unitty) (vecty (interval 0 200.0) '(2))
            (env `((regular ,(vecty (interval 0 200) '()))
                   (special ,(vecty (interval 0 200) '()))))
            (constraint (vec-dense '((0.25) (0.5))) '>= (vec-dense '(0  )))
            (constraint (vec-dense '((0.25) (0.5))) '<= (vec-dense '(60 )))
            (constraint (vec-dense '((1) (1)))      '>= (vec-dense '(0  )))
            (constraint (vec-dense '((1) (1)))      '<= (vec-dense '(200)))
            (constraint (vec-dense '((2) (1.25)))   '>= (vec-dense '(0  )))
            (constraint (vec-dense '((2) (1.25)))   '<= (vec-dense '(250)))
            (constraint (vec-dense '((2) (0.6)))    '>= (vec-dense '(0  )))
            (constraint (vec-dense '((2) (0.6)))    '<= (vec-dense '(240)))
            (constraint (vec-dense '((0) (0.5)))    '>= (vec-dense '(0  )))
            (constraint (vec-dense '((0) (0.5)))    '<= (vec-dense '(50 ))))))
    (vec-dense '(72.0 84.0)))
  )
