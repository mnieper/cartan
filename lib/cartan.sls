#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (cartan)
  (export
    define-variable
    expression-writer
    +
    *)
  (import
    (except (rnrs)
      +
      *)
    (scheme-libraries))

  (define-syntax/who define-variable
    (lambda (stx)
      (syntax-case stx ()
        [(_ id)
         (identifier? #'id)
         #'(begin
             (define x (make-variable 'id))
             (define-syntax id (identifier-syntax x)))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  ;; Expressions

  (define-record-type expression
    (opaque #t)
    (fields context))

  (define/who ->expression
    (lambda (obj)
      (cond
       [(number? obj) obj]
       [(symbol? obj)
        (cond
         [(context-ref-symbol (current-context) obj)]
         [else (assertion-violation who "undefined symbol" obj)])]
       [(expression? obj)
        (unless (eq? (expression-context obj)
                     (current-context))
          (assertion-violation who "expression out of context" obj))
        obj]
       [else
        (assertion-violation who "not an expression" obj)])))

  (define expression-writers '())

  (define/who expression-writer
    (lambda (rtd proc)
      (unless (record-type-descriptor? rtd)
        (assertion-violation who "invalid record type descriptor argument" rtd))
      (unless (procedure? proc)
        (assertion-violation who "invalid procedure argument" proc))
      (set! expression-writers
            (cons (cons (record-predicate rtd) proc)
                  expression-writers))))

  (define expression-write
    (lambda (r p wr)
      (let ([r (->expression r)])
        (if (number? r)
            (wr r p)
            (let f ([writers expression-writers])
              (if (null? writers)
                  (display "#<expression>")
                  (if ((caar writers) r)
                      ((cdar writers) r p wr)
                      (f (cdr writers)))))))))

  ;; Variables

  (define-record-type variable
    (parent expression)
    (fields name)
    (protocol
      (lambda (pargs->new)
        (lambda (name)
          (define ctx (current-context))
          (let ([var ((pargs->new ctx) name)])
            (context-declare-symbol! ctx name var)
            var)))))

  ;; Sums

  (define-record-type sum
    (parent expression)
    (fields summands)
    (protocol
      (lambda (pargs->new)
        (lambda (x*)
          ((pargs->new (current-context)) x*)))))

  (define/who +
    (lambda x*
      (let [(x* (map ->expression x*))]
        (cond
         [(null? x*)
          0]
         [(null? (cdr x*))
          (car x*)]
         [else (make-sum x*)]))))

  ;;  Products

  (define-record-type product
    (parent expression)
    (fields factors)
    (protocol
      (lambda (pargs->new)
        (lambda (x*)
          ((pargs->new (current-context)) x*)))))

  (define/who *
    (lambda x*
      (let [(x* (map ->expression x*))]
        (cond
         [(null? x*)
          1]
         [(null? (cdr x*))
          (car x*)]
         [else (make-product x*)]))))

  ;; Differentiation



  ;; Contexts

  (define-record-type context
    (opaque #t)
    (fields symbols)
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-symbol-table))))))

  (define make-symbol-table
    (lambda ()
      (make-hashtable symbol-hash symbol=?)))

  (define/who context-declare-symbol!
    (lambda (ctx sym val)
      (hashtable-update! (context-symbols ctx)
                         sym
                         (lambda (old-val)
                           (when old-val
                             (assertion-violation who "symbol already declared" ctx sym))
                           val)
                         #f)))

  (define context-ref-symbol
    (lambda (ctx sym)
      (hashtable-ref (context-symbols ctx) sym #f)))

  (define/who current-context
    (make-thread-parameter (make-context)
      (lambda (ctx)
        (unless (context? ctx)
          (assertion-violation who "invalid context" ctx))
        ctx)))

  ;; Record writers

  (record-writer (record-type-descriptor expression) expression-write)

  (expression-writer (record-type-descriptor variable)
    (lambda (r p wr)
      (wr (variable-name r) p)))

  (expression-writer (record-type-descriptor sum)
    (lambda (r p wr)
      (display "(+" p)
      (for-each
        (lambda (x)
          (write-char #\space p)
          (expression-write x p wr))
        (sum-summands r))
      (display ")" p)))

  (expression-writer (record-type-descriptor product)
    (lambda (r p wr)
      (display "(*" p)
      (for-each
       (lambda (x)
         (write-char #\space p)
         (expression-write x p wr))
       (product-factors r))
      (display ")" p)))

  )
