#lang racket

(require (for-syntax racket/struct-info syntax/transformer))
(require "contract-utils.rkt")
(require "ordered-hash.rkt")
(require "ty.rkt")

(provide
  envof
  (rename-out [env-ctor env]) env?
  env-entries
  env-empty env-empty? env-count
  env-dom env-range env-reorder
  env->ty env-has-key env-equiv?
  env-add env-set env-remove env-remove*
  env-inter-key env-diff-key env-union-key
  env-assoc env-filter-keys env-filter
  env-map env-map-vals)

(define/contract (unique-keys xs)
  (-> (listof (list/c any/c any/c)) boolean?)
  (not (check-duplicates (map car xs))))

(define (environment/c c)
  (and/c (listof (list/c symbol? c)) unique-keys))

(define (envof c?)
  (and/c env? (property/c env-range?
                (suggest/c
                  (curry contract-equivalent? c?)
                  "expected" (format "~a" c?)))))

(struct env
  (table range?)
  #:transparent)

(define/contract (make-env es r?)
  (->i ([es (r?) (environment/c r?)]
        [r? flat-contract?])
       [result env?])
  (env (list->ordered-hash es) r?))

(define-match-expander env-ctor
  (lambda (stx)
    (syntax-case stx ()
      [(_ es)    #'(env (app ordered-hash->list es) _)]
      [(_ es r?) #'(env (app ordered-hash->list es) r?)]))
  (lambda (stx)
    (syntax-case stx ()
      [(_)       #'(make-env '() ty?)]
      [(_ es)    #'(make-env es ty?)]
      [(_ es r?) #'(make-env es r?)]
      [_ #'make-env])))

(define/contract (env-entries e)
  (->i ([e env?])
       [result (e) (listof (list/c symbol? (env-range? e)))])
  (ordered-hash->list (env-table e)))

(define (env-empty [r? ty?]) (env (ordered-hash) r?))

(define/contract (env-empty? e)
  (-> env? boolean?)
  (ordered-hash-empty? (env-table e)))

(define/contract (env-count e)
  (-> env? natural?)
  (ordered-hash-count (env-table e)))

(define/contract (env-dom e)
  (-> env? (listof symbol?))
  (ordered-hash-keys (env-table e)))

(define/contract (env-range e)
  (->i ([e env?])
       [result (e) (listof (env-range? e))])
  (ordered-hash-values (env-table e)))

(define/contract (env-reorder ks e)
  (-> (listof symbol?) env? env?)
  (env (ordered-hash-reorder (env-table e) ks)
       (env-range? e)))

(define/contract (env->ty Gamma)
  (-> (envof ty?) ty?)
  (ty-list->ty (env-range Gamma)))

(define/contract (env-has-key x e)
  (-> symbol? env? boolean?)
  (ordered-hash-has-key? (env-table e) x))

(define/contract (env-equiv? e1 e2)
  (->i ([e1 env?]
        [e2 (e1) (envof (env-range? e1))])
       [result boolean?])
  (equal?
    (ordered-hash-table (env-table e1))
    (ordered-hash-table (env-table e2))))

(define/contract (env-add k v e)
  (->i ([k symbol?]
        [v (e) (env-range? e)]
        [e env?])
       [result env?])
  (let ((entry (env-assoc k e)))
    (if entry
      (if (equal? v (second entry))
        e
        (error (format "Variable ~v given conflicting values:~n~v~n~v" k v (second entry))))
      (env (ordered-hash-set (env-table e) k v)
           (env-range? e)))))

(define/contract (env-set k v e)
  (->i ([k symbol?]
        [v (e) (env-range? e)]
        [e env?])
       [result env?])
  (env (ordered-hash-set (env-table e) k v)
       (env-range? e)))

(define/contract (env-remove k e)
  (-> symbol? env? env?)
  (env (ordered-hash-remove (env-table e) k)
       (env-range? e)))

(define/contract (env-remove* ks e)
  (-> (listof symbol?) env? env?)
  (foldr env-remove e ks))

(define/contract (env-diff-key e1 e2)
  (-> env? env? env?)
  (env
    (foldr
      (lambda (k h)
        (ordered-hash-remove h k))
      (env-table e1)
      (env-dom e2))
    (env-range? e1)))

(define/contract (env-inter-key
                   #:combine [combine (lambda (v _) v)]
                   #:range? [range? #f]
                   e . es)
  (->i ([e env?])
       (#:combine [combine (or/c #f procedure?)]
        #:range?  [range? (or/c #f flat-contract?)])
       #:rest [es (e) (listof (envof (env-range? e)))]
       [result (e range?) (envof (or range? (env-range? e)))])
  (env (apply ordered-hash-intersect
         #:take-first #t
         #:combine combine
         (env-table e)
         (map env-table es))
       (or range? (env-range? e))))

(define/contract (env-union-key
                   #:combine [combine (lambda (v _) v)]
                   e . es)
  (->i ([e env?])
       (#:combine [combine (or/c #f procedure?)])
       #:rest [es (e) (listof (envof (env-range? e)))]
       [result (e) (envof (env-range? e))])
  (env (apply ordered-hash-union
         #:take-first #t
         #:combine combine
         (env-table e)
         (map env-table es))
       (env-range? e)))

(define/contract (env-assoc k e)
  (-> symbol? env? (or/c pair? #f))
  (let* ((not-found (gensym))
         (v (ordered-hash-ref (env-table e) k not-found)))
    (and (not (eq? v not-found)) (list k v))))

(define/contract (env-filter-keys xs e)
  (-> (listof symbol?) env? env?)
  (env
    (foldr
      (lambda (x h)
        (match (env-assoc x e)
          [`(,k ,v) (ordered-hash-set h k v)]
          [#f h]))
      (ordered-hash)
      xs)
    (env-range? e)))

(define (env-filter p? e)
  (env
    (ordered-hash-filter
      (env-table e)
      p?)
    (env-range? e)))

(define (env-map f e [range? (env-range? e)])
  (env
    (list->ordered-hash
      (ordered-hash-map
        (env-table e)
        f))
    range?))

(define (env-map-vals f e [range? (env-range? e)])
  (env
    (list->ordered-hash
      (ordered-hash-map
        (env-table e)
        (lambda (k v) `(,k ,(f v)))))
    range?))

(module+ test
  (require rackunit)
  (define tya (realty 2 2))
  (define tyb (bitty 4))
  (define tyc (polyty (bitty 4) '(1 1) (realty 2 2)))
  (define Ga (env-ctor `((a ,(realty 2 2)))))
  (define Gb (env-ctor `((b ,(bitty 4)))))
  (define Gc (env-ctor `((c ,(polyty (bitty 4) '(1 1) (realty 2 2))))))
  (define Gd (env-ctor `((d ,(unitty)))))
  (define Gabc (env-ctor `((a ,(realty 2 2))
                           (b ,(bitty 4))
                           (c ,(polyty (bitty 4) '(1 1) (realty 2 2))))))
  (check-equal? (env-entries Gabc)
                `((a ,tya) (b ,tyb) (c ,tyc)))
  (check-equal? (env-dom Gabc)
                `(a b c))
  (check-equal? (env-range Gabc)
                `(,tya ,tyb ,tyc))
  (check-equal? (env->ty Gabc)
                (tuplety tya tyb tyc))
  (check-true  (env-has-key 'b Gabc))
  (check-false (env-has-key 'd Gabc))
  (check-equal? (env-add 'a tya
                  (env-add 'b tyb
                    (env-add 'c tyc
                      (env-empty))))
                Gabc)
  (check-true (env-equiv? (env-add 'c tyc
                            (env-add 'b tyb
                              (env-add 'a tya
                                (env-empty))))
                          Gabc))
  (check-true (env-equiv? (env-set 'a tyb Ga)
                          (env-ctor `((a ,tyb)))))
  (check-true (env-equiv? (env-remove 'b (env-remove 'c Gabc))
                          Ga))
  (check-true (env-equiv? (env-remove* '(b c) Gabc)
                          Ga))
  (check-equal? (env-inter-key Gabc (env-union-key Ga Gd))
                Ga)
  (check-equal? (env-inter-key (env-union-key Gd Gabc) Gabc)
                Gabc)
  (check-equal? (env-inter-key Gb Gabc #:combine tuplety)
                (env-ctor `((b ,(tuplety (bitty 4) (bitty 4))))))
  (check-equal? (env-union-key Ga Gb Gc)
                Gabc)
  (check-equal? (env-union-key Ga (env-union-key Gb Gc))
                Gabc)
  (check-equal? (env-union-key (env-union-key Ga Gb) Gc)
                Gabc)
  (check-true (env-equiv? (env-union-key Ga Gb Gc)
                          Gabc))
  (check-true (env-equiv? (env-union-key Ga Gc Gb)
                          Gabc))
  (check-true (env-equiv? (env-union-key Gb Ga Gc)
                          Gabc))
  (check-true (env-equiv? (env-union-key Gb Gc Ga)
                          Gabc))
  (check-true (env-equiv? (env-union-key Gc Ga Gb)
                          Gabc))
  (check-true (env-equiv? (env-union-key Gc Gb Ga)
                          Gabc))
  (check-equal? (env-union-key Gb Gb #:combine tuplety)
                (env-ctor `((b ,(tuplety (bitty 4) (bitty 4))))))
  (check-equal? (second (env-assoc 'b Gabc))
                tyb)
  (check-false (env-assoc 'd Gabc))
  (check-equal? (env-filter-keys '(a) Gabc)
                Ga)
  (check-equal? (env-map (lambda (k _) `(,k 1)) Gabc number?)
                (env-ctor '((a 1) (b 1) (c 1)) number?))
  (check-equal? (env-map-vals (lambda (v) 1) Gabc number?)
                (env-ctor '((a 1) (b 1) (c 1)) number?))
  )
