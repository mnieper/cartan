#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (cartan)
  (export
    define-variable
    expression-writer
    *)
  (import
    (except (rnrs) *)
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
    (opaque #t))

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
      (if (number? r)
          (wr r p)
          (let f ([writers expression-writers])
            (if (null? writers)
                (display "#<expression>")
                (if ((caar writers) r)
                    ((cdar writers) r p wr)
                    (f (cdr writers))))))))

  ;; Variables

  (define-record-type variable
    (parent expression)
    (fields name))

  ;;  Products

  (define-record-type product
    (parent expression)
    (fields factors))

  (define/who *
    (lambda x*
      (for-all
       (lambda (x)
         (unless (or (number? x) (expression? x))
           (assertion-violation who "invalid expression" x)))
       x*)
      (cond
       [(null? x*)
        1]
       [(null? (cdr x*))
        (car x*)]
       [else (make-product x*)])))

  (record-writer (record-type-descriptor expression) expression-write)

  (expression-writer (record-type-descriptor variable)
    (lambda (r p wr)
      (wr (variable-name r) p)))

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
