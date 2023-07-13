#!r6rs

;;; Copyright © Marc Nieper-Wißkirchen (2023).


(library (cartan start)
  (export
    start)
  (import
    (rnrs)
    (scheme-libraries)
    (cartan eval))

  (define start
    (lambda ()
      (display "Cartan Version 0.0.1" (console-output-port))
      (newline (console-output-port))
      (repl eval))))
