#!r6rs
(library (cartan main)
  (export main)
  (import (rnrs))

  (define main
    (lambda (arg*)
      (banner)
      (let ([ctxt (make-context)])
        (repl ctxt))))

  (define banner
    (lambda ()
      (display "Cartan 0.0.1\n")
      (display "Copyright (C) 2024 Marc Nieper-Wißkirchen\n")
      (newline)))

  (define repl
    (lambda (ctxt)
      (let loop ()
        (let ([cmd (cartan:read)])
          (cond
           [(eof-object? cmd) (newline)]
           [else
            (print (eval cmd ctxt))
            (loop)])))))

  (define cartan:read
    (lambda ()
      (display "> ")
      (read)))

  (define print
    (lambda (e)
      (write e)
      (newline)))

  (define eval
    (lambda (cmd ctxt)
      (syntax-case (datum->syntax #'* cmd)
          (=
           distribute)
        [(= var ex)
         (identifier? #'var)
         (eval-= (syntax->datum #'var) (syntax->datum #'ex) ctxt)]
        [(distribute var ctxt)
         (identifier? #'var)
         (eval-distribute (syntax->datum #'var))]
        [_
         ;; Syntax error
         (assert #f)])))

  (define eval-=
    (lambda (var ex ctxt)
      (when (context-expression ctxt var)
        ;; Double definition
        (assert #f))
      ;; FIXME: Parse ex!
      (context-expression-set! ctxt var ex)
      `(= ,var ,ex)))

  (define eval-distribute
    (lambda (var ctxt)
      (let ([ex (context-expression ctxt var)])
        (unless ex
          (assert #f))
        (distribute ex))))

  (define distribute
    (lambda (ex)
      (cond
       ;; TODO: Handle other operators
       ;; How to handle renaming?  And how to handle hygiene.  Think kernel.
       [(product? ex)

        ]
       [else
        (expression-map distribute ex)])))

  (define expression-map
    (lambda (proc ex)
      (let f ([ex ex])
        (cond
         [(sum? ex)
          (let* ([e* (summands ex)]
                 [f* (map proc e*)])
            (if (for-all eqv? e* f*)
                ex
                (make-sum f*)))]
         [(product? ex)
          (let* ([e* (factors ex)]
                 [f* (map proc e*)])
            (if (for-all eqv? e* f*)
                ex
                (make-product f*)))]
         [else ex]))))

  (define sum?
    (lambda (ex)
      (and (pair? ex)
           (eqv? (car ex) '+))))

  (define make-sum
    (lambda (e*)
      `(+ ,@e*)))

  (define summands
    (lambda (sum)
      (cdr sum)))

  (define product?
    (lambda (ex)
      (and (pair? ex)
           (eqv? (car ex) '*))))

  (define make-product
    (lambda (e*)
      `(* ,@e*)))

  (define factors
    (lambda (prod)
      (cdr prod)))

  (define-record-type context
    (nongenerative context-705cfcdd-34e3-478a-a4bb-76b3978ec6db)
    (fields expressions)
    (protocol
      (lambda (new)
        (lambda ()
          (new (make-hashtable symbol-hash symbol=?))))))

  (define context-expression
    (lambda (ctxt var)
      (hashtable-ref (context-expressions ctxt) var #f))
    )

  (define context-expression-set!
    (lambda (ctxt var ex)
      (hashtable-set! (context-expressions ctxt) var ex))
    ))
