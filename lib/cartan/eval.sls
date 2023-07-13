#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).

(library (cartan eval)
  (export
    eval)
  (import
    (rnrs)
    (prefix (rnrs eval) rnrs:)
    (scheme-libraries))

  ;; FIXME
  (define eval values))
