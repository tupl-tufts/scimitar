#lang racket

(require ffi/unsafe)

(provide (rename-out [_string_enum* _string_enum]))

(define ((_string_enum name) symbols [basetype _symbol] #:unknown [unknown _string_enum])
  (unless (list? symbols)
    (raise-argument-error '_string_enum "list?" symbols))
  (when (and (procedure? unknown) (not (procedure-arity-includes? unknown 1)))
    (raise-argument-error '_string_enum "(if/c procedure? (procedure-arity-includes/c 1) any/c)" unknown))
  (define s->c
    (if name (string->symbol (format "enum:~a->string" name)) 'symbol->string))
  (define c->s
    (if name (string->symbol (format "enum:string->~a" name)) 'string->symbol))
  (for ([sym symbols])
    (unless (symbol? sym)
      (raise-arguments-error '_string_enum "key is not a symbol"
                             "symbols" symbols
                             "key" sym)))
  (make-ctype basetype
    (lambda (x)
      (if (member x symbols)
        x
        (raise-arguments-error s->c (format "argument does not fit ~a" (or name "enum")) 
                               "argument" x)))
    (lambda (x)
      (cond [(member x symbols) x]
            [(eq? unknown _string_enum)
             (error c->s "expected a known ~a, got: ~s" basetype x)]
            [(procedure? unknown) (unknown x)]
            [else unknown]))))

(define-syntax (_string_enum* stx)
  (syntax-case stx ()
    [(_ x ...)
     (with-syntax ([name (syntax-local-name)]) #'((_string_enum 'name) x ...))]
    [id (identifier? #'id) #'(_string_enum #f)]))
