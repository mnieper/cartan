(library (cartan)
  (export
    make-variable
    variable?
    variable-name
    define-variable
    (rename (%* *))
    product?
    product-factors
    flatten-product
    show)
  (import
    (rnrs))

  (define-record-type expression)

  (define-record-type variable
    (parent expression)
    (fields name))

  (define-record-type product
    (parent expression)
    (fields factors))

  (define %*
    (case-lambda
      [() 1]
      [(x) x]
      [(x y) (make-product (list x y))]
      [(x y z . z*) (make-product (cons* x y z z*))]))

  (define flatten-product
    (lambda (x)
      (apply
       %*
       (let f ([x x] [x* '()])
         (if (product? x)
             (fold-right f x* (product-factors x))
             (cons x x*))))))

  (define-syntax define-variable
    (lambda (stx)
      (syntax-case stx ()
        [(_ name)
         (identifier? #'name)
         #'(define name (make-variable 'name))])))

  (define show
    (case-lambda
      [(x)
       (show #t x)]
      [(p x)
       (case p
         [(#t) (show (current-output-port) x)]
         [(#f) (let-values ([(p get) (open-string-output-port)])
                 (show p x)
                 (get))]
         [else
          (let wr ([x x])
            (cond
             [(not (expression? x))
              (display x p)]
             [(variable? x)
              (wr (variable-name x))]
             [(product? x)
              (let [(x* (product-factors x))]
                (wr "(")
                (do ([x* (product-factors x) (cdr x*)]
                     [sep "" "*"])
                    ((null? x*))
                  (wr sep)
                  (wr (car x*)))
                (wr ")"))]
             [else
              (assert #f)]))])])))
