#lang racket

(require "../contract-utils.rkt")
(require "../env.rkt")
(require "../poly.rkt")
(require "../ty.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "../vec.rkt")
(require "grammar.rkt")

(provide
  free-in
  subst
  remove
  fix-names
  calculate-obj
  fun-app-obj-hack)

(define/contract (free-in e)
  (-> cps-cexp? (listof symbol?))
  (remove-duplicates
    (match e
      [(cps-app f args)
       (cons f (sym-val-free-in args))]
      [(cps-fix fs cont)
       (let ((ys (map car fs))
             (fvs (apply append
                    (free-in cont)
                    (map (match-lambda
                           [`(,_ ,xs ,b)
                            (remove* (flatten xs) (free-in b))])
                      fs))))
         (remove* ys fvs))]
      [(cps-primop _ args out cont)
       (append
         (sym-val-free-in args)
         (remove* (sym-val-free-in out) (free-in cont)))]
      [(cps-switch _ v conts)
       (let ((xs (concat (map free-in conts))))
         (append (sym-val-free-in v) xs))])))

(define/contract (subst x y e)
  (-> symbol? sym-val? cps-cexp? cps-cexp?)
  (define (sub z)
    (if (eq? x z) y z))
  (define (found-x? y)
    (define (go z acc)
      (or acc (eq? x z)))
    (tree-fold go #f y))
  (define (do-fs fs)
    (if (null? fs)
      #f
      (match* ((car fs) (do-fs (cdr fs)))
        [(`(,f ,xs ,b) fs2)
         (let* ((xs-shadowed (found-x? xs))
                (b (and (not xs-shadowed) (go b))))
           (and (or b fs2)
             (cons (if b `(,f ,xs ,b) (car fs))
                   (or fs2 (cdr fs)))))])))
  (define/match (go e)
    [((cps-app f args))
     (let ((found-f (found-x? f))
           (found-a (found-x? args)))
       (and (or found-f found-a)
            (cps-app (if found-f (sub f) f)
                     (if found-a (tree-map sub args) args))))]
    [((cps-fix fs cont))
     (if (assoc x fs)
       e
       (let ((new-fs (do-fs fs))
             (new-c (go cont)))
         (and (or new-fs new-c)
           (cps-fix (or new-fs fs) (or new-c cont)))))]
    [((cps-primop (cps-op dir obj p) args out cont))
     (let ((found-o (found-x? obj))
           (found-p (env-has-key x (poly-Gamma-col p)))
           (found-a (found-x? args))
           (new-c (and (not (member x (sym-val-free-in out))) (go cont))))
       (and (or found-o found-p found-a new-c)
         (cps-primop
           (cps-op dir (if found-o (map-car sub obj) obj)
                   (if found-p
                     (match p
                       [(poly ti to Gamma cs) ;; HARDCORE SUBSTITUTION
                        (poly ti to (env-map (lambda (x v) `(,(sub x) ,v)) Gamma) cs)])
                     p))
           (if found-a (tree-map sub args) args)
           out
           (or new-c cont))))]
    [((cps-switch op v conts))
     (let* ((found-v (found-x? v))
            (new-cs (map go conts))
            (found-c (ormap identity new-cs)))
       (and (or found-v found-c)
            (cps-switch op
              (if found-v (tree-map sub v) v)
              (if found-c (tree-map (lambda (n c) (or n c)) new-cs conts) conts))))])
  (or (go e) e))

(define (remove f e)
  (define/match (go e)
    [((cps-app _ _))
     e]
    [((cps-fix fs cont))
     (if (assoc f fs)
       (cps-fix (filter (compose not (curry equal? f) car) fs) cont)
       (cps-fix (map-caddr go fs) (go cont)))]
    [((cps-primop op args out cont))
     (cps-primop op args out (go cont))]
    [((cps-switch op v conts))
     (cps-switch op v (map go conts))])
  (go e))

(define/match (fix-names e)
  [((? cps-app?))
   '()]
  [((cps-fix fs cont))
   (apply append
     (map car fs)
     (fix-names cont)
     (map (compose fix-names caddr) fs))]
  [((cps-primop _ _ _ cont))
   (fix-names cont)]
  [((cps-switch _ _ conts))
   (concat (map fix-names conts))])

(define/contract (calculate-obj poly obj)
  (-> poly? (listof (list/c symbol? number?)) vec?)
  (let ((width (poly-width poly)))
    (apply vec+
      (vec-0 `(,width)) ;; for (null? obj)
      (map
        (lambda (xn)
          (match (env-assoc (car xn) (poly-Gamma-col poly))
            [`(,_ ,ty)
             (let ((cix (poly-column-index (car xn) poly)))
               (apply vec+
                 (vec-0 `(,width)) ;; for (= 0 (ty-dim ty))
                 (build-list
                   (ty-dim ty)
                   (lambda (i)
                     (vec.* (cadr xn)(vec-e `(,(+ i cix)) `(,width)))))))]
            [#f (error (format "Tried to solve for variable not found in poly args or results: ~a" (car xn)))]))
        obj))))

(define/contract (fun-app-obj-hack ty obj)
  (-> polyty? vec? vec?)
  ;; there's no way to get this offset from ir-compile, so hardcode it
  (let* ((ty-i (polyty-dom ty))
         (width (cadr (polyty-shape ty)))
         (ty-o (polyty-cod ty))
         (obj (vec-augment 0 (vec-0 `(,(+ (ty-dim ty-o) (ty-dim ty-i)))) obj)))
    (when (not (= width (vec-length obj)))
      (error "fun-app-obj-hack has failed"))
    obj))
