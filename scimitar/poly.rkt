#lang racket

(require racket/struct)
(require "contract-utils.rkt")
(require "env.rkt")
(require "vec.rkt")
(require "topo.rkt")
(require "ty.rkt")
(require "util.rkt")

(provide
  (except-out (struct-out constraint) constraint)
  (rename-out [constraint-ctor constraint])
  ineq->cmp
  constraint-pad
  constraint-augment constraint-append constraint-concat
  constraint-remove-row constraint-remove-col
  constraint->string string->constraint
  (except-out (struct-out poly) poly)
  (rename-out [poly-ctor poly])
  poly-all-types poly-column-types poly-column-index
  poly-optimize
  poly-empty
  poly-length poly-width poly-output-dim poly-input-dim poly-body-width)

(struct/contract
  constraint ([A (vec-dims/c (=/c 2))]
              [ineq (listof (symbols '<= '= '>=))]
              [b (vec-dims/c (=/c 1))])
  #:transparent)

(define/contract (constraint-ctor A ineq b)
  (->i ([A (vec-dims/c (=/c 2))]
        [ineq (or/c (listof (symbols '<= '= '>=)) (symbols '<= '= '>=))]
        [b (A) (and/c (vec-dims/c (=/c 1)) ((on/c (compose car vec-shape) =/c) A))])
       [result? constraint?])
  (constraint A (if (list? ineq) ineq (replicate (vec-length b) ineq)) b))

(define ineq->cmp
  (match-lambda ['<= <=] ['= =] ['>= >=]))

(define ineq->string
  (match-lambda ['<= "<="] ['= "="] ['>= ">="]))

(define string->ineq
  (match-lambda ["<=" '<=] ["=" '=] [">=" '>=] [s (error (format "Unknown value in string->ineq: ~v" s))]))

(define/contract (constraint-pad c s)
  (->i ([c constraint?]
        [s (c) (and/c (=/on-dims/c (vec-shape (constraint-A c)))
                      (measurement-exceeds (vec-shape (constraint-A c))))])
       [result constraint?])
  (match c
    [(constraint A ineq b)
     (constraint (vec-pad A s) ineq (vec-pad b (take 1 s)))]))

(define/contract (constraint-augment A c)
  (->i ([A (or/c vec? constraint?)]
        [c (A) (if (vec? A)
                 (and/c constraint?
                   (property/c constraint-A ((on/c (compose car vec-shape) =/c) A)))
                 ((on/c (compose car vec-shape) =/c) (constraint-A A)))])
       [result constraint?])
  (let ((c (if (vec? c) A c))
        (A (if (vec? A) A (constraint-A A)))
        (B (if (vec? c) c (constraint-A c))))
    (constraint
      (vec-augment 1 A B)
      (constraint-ineq c)
      (constraint-b c))))

(define/contract (constraint-append c . cs)
  (-> constraint? constraint? ... constraint?)
  (constraint-concat (cons c cs)))

(define/contract (constraint-concat cs)
  (-> (listof constraint?) constraint?)
  (constraint
    (let* ((As (map constraint-A cs))
           (w-max (foldl (lambda (A w) (max w (cadr (vec-shape A)))) 0 As))
           (do-pad (lambda (A) (vec-pad A `(,(car (vec-shape A)) ,w-max)))))
      (apply vec-augment 0 (vec-0 `(0 ,w-max)) (map do-pad As)))
    (apply append (map constraint-ineq cs))
    (apply vec-augment 0 (vec-0) (map constraint-b cs))))

(define/contract (constraint-remove-row c n)
  (-> constraint? nonnegative-integer? constraint?)
  (define (remove-n es)
    (filter identity
      (map (match-lambda
             [(and e `((,m . ,cs) ,v))
              (if (>= m n)
                (and (> m n)
                     `((,(- m 1) . ,cs) ,v))
                e)])
           es)))
  (match c
    [(constraint
       (vec-sparse A-es (vecty A-i `(,A-r ,A-c)))
       ineq
       (vec-sparse b-es (vecty b-i `(,b-r))))
     (constraint
       (vec-sparse (remove-n A-es) (vecty A-i `(,(- A-r 1) ,A-c)))
       (omit ineq n)
       (vec-sparse (remove-n b-es) (vecty b-i `(,(- b-r 1)))))]))

(define/contract (constraint-remove-col c n)
  (-> constraint? nonnegative-integer? constraint?)
  (define (remove-n es)
    (filter identity
      (map (match-lambda
             [(and e `((,j ,m) ,v))
              (if (>= m n)
                (and (> m n)
                     `((,j ,(- m 1)) ,v))
                e)])
           es)))
  (match c
    [(constraint (vec-sparse A-es (vecty A-i `(,A-r ,A-c))) ineq b)
     (constraint
       (vec-sparse (remove-n A-es) (vecty A-i `(,A-r ,(- A-c 1)))) ineq b)]))

(define/contract (constraint->string c)
  (-> constraint? string?)
  (let ((As (string-split (vec->string (constraint-A c)) "\n"))
        (ineqs (map ineq->string (constraint-ineq c)))
        (bs (string-split (vec->string (constraint-b c)) "\n")))
    (apply string-append
      (add-between
        (map (lambda (A ineq b)
               (string-append A " " ineq " " b))
             As ineqs bs)
        "\n"))))

(define/contract (string->constraint s)
  (-> string? constraint?)
  (let* ((lines (string-split s "\n"))
         (regions (map (lambda (line) (map string-trim (string-split line #rx"(?= <=| >=| =)|(?<==)"))) lines))
         (_ (and (ormap null? regions) (error "Constraints must be over a rank 2 matrix")))
         (A (string->vec (string-join (map car   regions) "\n")))
         (ineq (map string->ineq      (map cadr  regions)))
         (b (string->vec (string-join (map caddr regions) "\n"))))
    (constraint A ineq b)))

(struct/contract
  poly ([ty polyty?]
        [Gamma-col env?]
        [cs constraint?])
  #:property prop:custom-write
  (make-constructor-style-printer
    (lambda _ 'poly)
    (lambda (self)
      (list (polyty-dom (poly-ty self))
            (polyty-cod (poly-ty self))
            (poly-Gamma-col self)
            (poly-cs self))))
  #:property prop:custom-print-quotable 'never
  #:transparent)

(define (cs-dims cs)
  (vec-shape (constraint-A cs)))

(define (make-poly ty-i ty-o Gamma . cs)
  (let* ((cs (constraint-concat cs))
         (dims (cs-dims cs))
         (p (poly (polyty ty-i dims ty-o) Gamma cs)))
    (if (>= (length (poly-column-types p))
            (cadr dims))
      p
      (error "Poly types do not match definition"))))

(define-match-expander poly-ctor
  (lambda (stx)
    (syntax-case stx ()
      [(_ ty-i ty-o Gamma c) #'(poly (polyty ty-i _ ty-o) Gamma c)]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ ty-i ty-o Gamma c ...)
       #'(make-poly ty-i ty-o Gamma c ...)]
      [_ #'make-poly])))

(define/contract (poly-all-types p)
  (-> poly? (listof ty?))
  (env-range (poly-Gamma-col p)))

(define/contract (poly-column-types p)
  (-> poly? (listof ty?))
  (concat (map ty->scalar-ty-list
               (poly-all-types p))))

(define/contract (poly-column-index col p)
  (-> symbol? poly? integer?)
  (let* ((cols (env-dom (poly-Gamma-col p)))
         (offsets (scanl + 0 (map ty-dim (env-range (poly-Gamma-col p))))))
    (second (or (assoc col (zip cols offsets))
                (error (format "Column named ~a not found in poly" col))))))

(define/contract (poly-optimize p)
  (-> poly? poly?)
  (let ((cs (foldr
              (match-lambda*
                [`(,n ,(and (constraint A ineq b) c))
                 (let* ((w (vec-width A))
                        (l (vec-length A))
                        (row (vec-project A `(,n 0) `(,(+ 1 n) ,w)))
                        (row-b (vec! b `(,n))))
                   (if (vec-null? row)
                     (if (= 0 row-b)
                       (constraint-remove-row c n)
                       (error "Encountered unsatisfiable constraint while optimizing"))
                     (match-let* ([(vec-sparse es) row]
                                  [`((0 ,m) ,mag) (last es)]
                                  [(vec-sparse cs) (vec-project A `(0 ,m) `(,l ,(+ 1 m)))])
                       (if (< m (+ (poly-output-dim p) (poly-input-dim p)))
                         c ;; Only optimize "local" vars
                         (let* ((ns (map caar cs))
                                (scales (map (compose (curryr / mag) cadr) cs))
                                (rows (map (curryr vec.* row) scales))
                                (V (vec-pad
                                     (foldl (lambda (scale n V)
                                              (vec-augment 0
                                                V
                                                (vec-0 `(,(- n (vec-length V)) ,w))
                                                (vec.* scale row)))
                                            (vec-0 `(0 ,w)) scales ns)
                                     `(,l ,w)))
                                (u (apply vec+
                                     (map (lambda (scale n)
                                            (vec.* (* scale row-b) (vec-e `(,n) `(,l))))
                                          scales ns))))
                           (if (>= 1 (length ns))
                             c ;; Only optimize when there is more than one row.
                             (if (andmap (curry eq? '=) (map (curry list-ref ineq) ns))
                               c ;; Only optimize vars that are entirely within equalities
                               (constraint-remove-row
                                 (constraint (vec- A V) ineq (vec- b u)) n))))))))])
              (poly-cs p)
              (indexes-of (constraint-ineq (poly-cs p)) '=))))
    (poly
      (polyty (polyty-dom (poly-ty p)) (cs-dims cs) (polyty-cod (poly-ty p)))
      (poly-Gamma-col p)
      cs)))

(define poly-empty
  (poly-ctor (unitty) (unitty) (env-empty)
    (constraint-ctor (vec-0 '(0 0)) '= (vec-0 '(0)))))

(define (poly-length p)
  (match (poly-ty p)
    [(polyty _ (list l _) _) l]))

(define (poly-width p)
  (match (poly-ty p)
    [(polyty _ (list _ w) _) w]))

(define (poly-output-dim p)
  (match p
    [(poly-ctor _ ty-o _ _) (ty-dim ty-o)]))

(define (poly-input-dim p)
  (match p
    [(poly-ctor ty-i _ _ _) (ty-dim ty-i)]))

(define (poly-body-width p)
  (- (poly-width p) (poly-output-dim p) (poly-input-dim p)))
