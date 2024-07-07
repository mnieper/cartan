#!chezscheme
(library (cartan)
  (export
    ;; Expressions
    expression
    unquote
    define-expression-syntax
    type
    expression-type
    expression<?
    ;; Variables
    make-variable
    variable?
    variable-name
    define-variable
    ;; Products
    *
    product?
    product-left
    product-right
    flatten-product
    ;; Sums
    +
    sum?
    sum-left
    sum-right
    flatten-sum
    distribute-product
    collect-sum
    ;; Output
    show)
  (import
    (chezscheme))

  ;; Expressions

  (define-record-type complex-expression)

  (define-syntax expression-transformer
    (lambda (stx)
      (define who 'expression-transformer)
      (syntax-violation who "invalid syntax" stx)))

  (define-syntax define-expression-syntax
    (lambda (stx)
      (syntax-case stx ()
        [(_ name transformer)
         (identifier? #'name)
         #'(define-property name expression-transformer
             (let ([proc transformer])
               (unless (procedure? proc)
                 (assertion-violation 'define-expression-syntax "invalid transformer" proc))
               proc))])))

  (define-syntax expression
    (lambda (stx)
      (define who 'expression)
      (syntax-case stx ()
        [(_ x)
         (lambda (lookup)
           (let ([x #'x])
             (syntax-case x (unquote)
               [(unquote e) #'e]
               [(k a ...)
                (identifier? #'k)
                (cond
                 [(lookup #'k #'expression-transformer) =>
                  (lambda (proc)
                    (proc x))]
                 [else
                  (syntax-violation who "invalid expression syntax" stx x)])]
               [k
                (identifier? #'k)
                #''k]
               [e
                (number? (syntax->datum #'e))
                #'e]
               [_ (syntax-violation who "invalid expression syntax" stx x)])))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  (define-enumeration type
    (real variable product sum)
    type-set)

  (define expression-type
    (lambda (x)
      (cond
       [(real? x) (type real)]
       [(symbol? x) (type variable)]
       [(product? x) (type product)]
       [(sum? x) (type sum)]
       [else (assert #f)])))

  (define expression<?
    (lambda (x y)
      (cond
       [(real? x)
        (or (not (real? y))
            (< x y))]
       [(real? y) #f]
       [(product? x) (product<? x y)]
       [(product? y) (product<? x y)]
       [(or (sum? x) (sum? y))
        (sum<? x y)]
       [(symbol<? x y)])))

  (define expression*<?
    (lambda (x* y*)
      (let f ([x* (reverse x*)]
              [y* (reverse y*)])
        (cond
         [(null? x*) (not (null? y*))]
         [(null? y*) #f]
         [else
          (let ([x (car x*)] [x* (cdr x*)]
                [y (car y*)] [y* (cdr y*)])
            (or (expression<? x y)
                (and (not (expression<? y x))
                     (f x* y*))))]))))

  (define symbol<?
    (lambda (x y)
      (string<? (symbol->string x) (symbol->string y))))

  (define product<?
    (lambda (x y)
      (expression*<? (factors x) (factors y))))

  (define sum<?
    (lambda (x y)
      (expression*<? (summands x) (summands y))))

  ;; Variables

  (define-record-type variable
    (parent complex-expression)
    (fields name))

  (define-syntax define-variable
    (lambda (stx)
      (syntax-case stx ()
        [(_ name)
         (identifier? #'name)
         #'(define name (make-variable 'name))])))

  ;; Products

  (define-record-type product
    (parent complex-expression)
    (fields left right))

  (define mul
    (case-lambda
      [() 1]
      [(x) x]
      [(x y)
       (if (eqv? y 1)
           x
           (make-product x y))]
      [(x y z . z*)
       (make-product x
                     (let f ([y y] [z z] [z* z*])
                       (if (null? z*)
                           (mul y z)
                           (mul y (f z (car z*) (cdr z*))))))]))

  (define-expression-syntax *
    (lambda (stx)
      (syntax-case stx ()
        [(_ x ...)
         #'(mul (expression x) ...)])))

  (define factors
    (lambda (x)
      (cond
       [(eqv? x 1) (list)]
       [(product? x)
        (cons (product-left x) (factors (product-right x)))]
       [else (list x)])))

  (define flatten-product
    (lambda (x)
      (if (product? x)
          (let f ([y (product-left x)]
                  [z (flatten-product (product-right x))])
            (if (product? y)
                (f (product-left y) (f (product-right y) z))
                (make-product y z)))
          x)))

  ;; Sums

  (define-record-type sum
    (parent complex-expression)
    (fields left right))

  (define add
    (case-lambda
      [() 0]
      [(x) x]
      [(x y)
       (if (eqv? y 0)
           x
           (make-sum x y))]
      [(x y z . z*)
       (make-sum x
                 (let f ([y y] [z z] [z* z*])
                   (if (null? z*)
                       (add y z)
                       (add y (f z (car z*) (cdr z*))))))]))

  (define-expression-syntax +
    (lambda (stx)
      (syntax-case stx ()
        [(_ x ...)
         #'(add (expression x) ...)])))

  (define flatten-sum
    (lambda (x)
      (if (sum? x)
          (let f ([y (sum-left x)]
                  [z (flatten-sum (sum-right x))])
            (if (sum? y)
                (f (sum-left y) (f (sum-right y) z))
                (make-sum y z)))
          x)))

  (define summands
    (lambda (x)
      (cond
       [(eqv? x 0) (list)]
       [(sum? x)
        (cons (sum-left x) (summands (sum-right x)))]
       [else (list x)])))

  (define distribute-product
    (lambda (x)
      (if (product? x)
          (let ([y* (summands (product-left x))]
                [z* (summands (distribute-product (product-right x)))])
            (fold-right
             (lambda (y x*)
               (fold-right
                (lambda (z x*)
                  (add (make-product y z) x*))
                x* z*))
             0 y*))
          x)))

  (define collect-sum
    (lambda (x)
      (fold-right
       (lambda (y z)
         (let f ([y y] [z z])
           (cond
            [(sum? z)
             (let ([w (sum-left z)])
               (cond
                ;; FIXME: Add like terms.
                [(expression<? y w)
                 (make-sum y z)]
                [(expression<? w y)
                 (make-sum w (f y (sum-right z)))
                 ]
                [else
                 ;; FIXME: Put together.
                 (make-sum y z)]))]
            [(eqv? z 0)
             y]
            [else
             (cond
              ;; FIXME: Add like terms.
              [(expression<? y z)
               (make-sum y z)]
              [(expression<? z y)
               (make-sum z y)]
              [else
               ;; FIXME: Put together.
               (make-sum y z)])])))
       0 (summands (flatten-sum x)))))

  ;; Output

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
          (let wr/rank ([x x] [rank 0])
            (let wr ([x x])
              (cond
               [(not (complex-expression? x))
                (display x p)]
               [(variable? x)
                (wr (variable-name x))]
               [(product? x)
                (when (fx>? rank 1)
                  (wr "("))
                (let f ([x x])
                  (wr/rank (product-left x) 1)
                  (wr "*")
                  (let ([y (product-right x)])
                    (cond
                     [(product? y) (f y)]
                     [else (wr/rank y 1)])))
                (when (fx>? rank 1)
                  (wr ")"))]
               [(sum? x)
                (when (fx>? rank 0)
                  (wr "("))
                (let f ([x x])
                  (wr/rank (sum-left x) 1)
                  (wr "+")
                  (let ([y (sum-right x)])
                    (cond
                     [(sum? y) (f y)]
                     [else (wr/rank y 1)])))
                (when (fx>? rank 0)
                  (wr ")"))]
               [else
                (assert #f)])))])]))

  (record-writer (record-type-descriptor complex-expression)
    (lambda (r p wr)
      (display-string "#<expression " p)
      (show p r)
      (display-string ">" p))))
