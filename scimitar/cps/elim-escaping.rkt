#lang racket

(require "../contract-utils.rkt")
(require "../util.rkt")
(require "../val.rkt")
(require "alpha.rkt")
(require "basis.rkt")
(require "grammar.rkt")
(require "util.rkt")

(provide
  (prefix-out cps- elim-escaping))

(define local-var (string->unreadable-symbol "local"))

(define (resolve v as)
  (cond
    [(symbol? v)
     (match (assoc v as)
       [`(,_ ,v) v]
       [#f v])]
    [(list? v) (map (curryr resolve as) v)]
    [else v]))

;; NOTE: this pass temporarily makes the program incorrect,
;;       because it inlines function arguments that may not
;;       currently be in scope.  This is repaired in flattening.
;; NOTE: This pass relies on globally unique names
(define/contract (elim-escaping e)
  (-> cps-cexp? cps-cexp?)
  (define fxs (box (fix-names e)))
  (define (box-cons x xs)
    (set-box! xs (cons x (unbox xs))))
  (define (add-fix-name f)
    (box-cons f fxs))
  (define (fix-name? arg)
    (and (member arg (unbox fxs)) #t))
  (define (concrete? arg)
    (or (val? arg) (fix-name? arg)))
  (define (concretize args)
    (tree-map
      (lambda (arg)
        (if (concrete? arg)
          arg
          local-var))
      args))
  (define (fix-in-value v)
    (filter fix-name? (sym-val-free-in v)))

  (define (clone f g hs e-raw e)
    (define/match (go e-raw e)
      [((cps-app _ _) (cps-app _ _))
       (values e-raw e)]
      [((cps-fix fs-raw cont-raw) (cps-fix fs cont))
       (match* ((assoc f fs-raw) (assoc f fs))
         [(`(,_ ,xs ,b-raw) `(,_ ,_ ,_))
          ;; This strange construction is to avoid relying on the alpha renaming order within fs
          (match (cps-alpha (cps-fix `((,f ,xs ,b-raw))
                                     (cps-fix `((,g ,xs ,(foldl remove (subst f g b-raw) hs)))
                                              cont)))
            [(cps-fix _ (cps-fix `(,gysb) _))
             (for-each add-fix-name
               (cons g (fix-names (caddr gysb))))
             (values
               (cps-fix (snoc fs-raw gysb) cont-raw)
               (cps-fix (snoc fs     gysb) cont))])]
         [(#f #f)
          (let-values ([(fs-raw fs)
                        (for/lists (fs-raw fs) ([f-raw fs-raw] [f fs])
                          (let-values ([(b-raw b) (go (caddr f-raw) (caddr f))])
                            (values
                              `(,(car f-raw) ,(cadr f-raw) ,b-raw)
                              `(,(car f    ) ,(cadr f    ) ,b    ))))]
                       [(cont-raw cont) (go cont-raw cont)])
            (values
              (cps-fix fs-raw cont-raw)
              (cps-fix fs     cont)))])]
      [((cps-primop op-raw args-raw out-raw cont-raw) (cps-primop op args out cont))
       (let-values ([(cont-raw cont) (go cont-raw cont)])
         (values
           (cps-primop op-raw args-raw out-raw cont-raw)
           (cps-primop op     args     out     cont)))]
      [((cps-switch op-raw v-raw conts-raw) (cps-switch op v conts))
       (let-values ([(conts-raw conts) (map-values go conts-raw conts)])
         (values
           (cps-switch op-raw v-raw conts-raw)
           (cps-switch op     v     conts)))])
    (go e-raw e))

  (define (dispatch p-raw p f args rho calls k)
    (define/match (do-list es-raw es do-one k)
      [('() '() _ _)
       (k es-raw es calls #f)]
      [((cons e-raw es-raw) (cons e es) _ _)
       (do-one e-raw e
         (lambda (e-raw e calls found)
           (if found
             (k (cons e-raw es-raw) (cons e es) calls found)
             (do-list es-raw es do-one
               (lambda (es-raw es calls found)
                 (k (cons e-raw es-raw)
                    (cons e es)
                    calls found))))))])
    (define/match (inner e-raw e k)
      [(e-raw (cps-app _ _) _)
       (k e-raw e calls #f)]
      [((cps-fix fs-raw cont-raw) (cps-fix fs cont) _)
       (do-list fs-raw fs
         (if (assoc f fs)
           (lambda (fxsb-raw fxsb k)
             (match* (fxsb-raw fxsb)
               [(`(,g-raw ,xs-raw ,_) `(,g ,xs ,b))
                (if (equal? g f)
                  (let* ((rho-g (tree-preorder-zip xs args))
                         ; TODO: this is incomplete for the case where xs = 'x and args = '(a b) where one of a or b are concrete
                         (rho-c (filter (compose concrete? cadr) rho-g))
                         (_ (when (ormap (compose list? car) rho-c)
                              (error "During CPS escape elimination, tried to assign a concrete argument to multiple parameters")))
                         (rho (append (filter (compose symbol? car) rho-c) rho)))
                    (run p-raw b rho calls
                      (lambda (b-raw b calls)
                        (k `(,g-raw ,xs-raw ,b-raw)
                           `(,g ,xs ,b)
                           calls #t))))
                  (k fxsb-raw fxsb calls #f))]))
           (lambda (fxsb-raw fxsb k)
             (match* (fxsb-raw fxsb)
               [(`(,g-raw ,xs-raw ,b-raw) `(,g ,xs ,b))
                (inner b-raw b
                  (lambda (b-raw b calls found)
                    (k `(,g-raw ,xs-raw ,b-raw)
                       `(,g ,xs ,b)
                       calls found)))])))
         (lambda (fs-raw fs calls found)
           (if found
             (k (cps-fix fs-raw cont-raw)
                (cps-fix fs cont)
                calls found)
             (inner cont-raw cont
               (lambda (cont-raw cont calls found)
                 (k (cps-fix fs-raw cont-raw)
                    (cps-fix fs cont)
                    calls found))))))]
      [((cps-primop op-raw args-raw out-raw cont-raw) (cps-primop op args out cont) _)
       (inner cont-raw cont
         (lambda (cont-raw cont calls found)
           (k (cps-primop op-raw args-raw out-raw cont-raw)
              (cps-primop op args out cont)
              calls found)))]
      [((cps-switch op-raw v-raw conts-raw) (cps-switch op v conts) _)
       (do-list conts-raw conts inner
         (lambda (conts-raw conts calls found)
           (k (cps-switch op-raw v-raw conts-raw)
              (cps-switch op v conts)
              calls found)))])
    (inner p-raw p
      (lambda (p-raw p calls found)
        (if found
          (k p-raw p calls)
          (error "Bug in CPS escape elimination")))))

  (define (run p-raw e rho calls k)
    (define/match (do-list es calls k)
      [('() _ _)
       (k es es calls)]
      [((cons e es) _ _)
       (inner e calls
         (lambda (e-raw e calls)
           (do-list es calls
             (lambda (es-raw es calls)
               (k (cons e-raw es-raw)
                  (cons e es)
                  calls)))))])
    (define/match (inner e calls k)
      [((cps-app f args) _ _)
       #:when (cps-exit? (resolve f rho))
       (k e (cps-app cps-exit-sym (resolve args rho)) calls)]
      [((cps-app f args) _ _)
       (let ((f (resolve f rho))
             (args (resolve args rho)))
         (when (not (concrete? f))
           (error (format (string-append "During CPS escape elimination:\n"
                                         "  Could not eliminate closure at call to variable ~a\n"
                                         "  This is likely a call to the output of a primitive\n"
                                         "  Full closures needed to handle this") f)))
         (match (assoc f calls)
           [`(,_ ,c-args)
            (if (equal? (concretize args) c-args)
              (k e (cps-app f args) calls)
              (let*-values ([(g) (gensym f)]
                            [(calls) (cons `(,g ,(concretize args)) calls)]
                            [(p-raw p calls) (k e (cps-app g args) calls)]
                            [(p-raw p) (clone f g (fix-in-value args) p-raw p)])
                (dispatch p-raw p g args rho calls values)))]
           [#f
            (let*-values ([(calls) (cons `(,f ,(concretize args)) calls)]
                          [(p-raw p calls) (k e (cps-app f args) calls)])
              (dispatch p-raw p f args rho calls values))]))]
      [((cps-fix fs cont) _ _)
       (inner cont calls
         (lambda (cont-raw cont calls)
           (k (cps-fix fs cont-raw)
              (cps-fix fs cont)
              calls)))]
      [((cps-primop op args out cont) _ _)
       (inner cont calls
         (lambda (cont-raw cont calls)
           (k (cps-primop op args out cont-raw)
              (cps-primop op (resolve args rho) out cont)
              calls)))]
      [((cps-switch op v conts) _ _)
       (do-list conts calls
         (lambda (conts-raw conts calls)
           (k (cps-switch op v conts-raw)
              (cps-switch op (resolve v rho) conts)
              calls)))])
    (inner e calls k))

  (let-values ([(p-raw p calls) (run e e '() '() values)])
    p))
