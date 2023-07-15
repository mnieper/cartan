#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (cartan eval)
  (export
    eval)
  (import
    (rnrs)
    (only (rnrs r5rs)
      delay
      force)
    (prefix (rnrs eval) rnrs:)
    (scheme-libraries)
    (cartan parse))

  ;; FIXME
  (define/who eval
    (define environment
      (delay (rnrs:environment '(rnrs))))
    (lambda (x)
      (rnrs:eval (parse x) (force environment)))))
