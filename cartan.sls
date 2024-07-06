(library (cartan)
  (export
    make-variable
    variable?
    variable-name)

  (define-record-type variable
    (fields name))

  ;; Problem: How can we extend this?
  (define show
    (lambda (x)
      (cond
       ((variable? x) (variable-name name))
       ...)))




  )
