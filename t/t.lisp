(defpackage :compiler-t
  (:use cl)
  (:use compiler.lexer)
  (:export :t1))

(in-package :compiler-t)

(defun t1 (filename)
  (let ((compiler.lexer::*stream* (open filename)))
    ;; TODO: is there a way to accomplish the do loop with the standard loop macro.
    (do ((r (compiler.lexer::peek) (compiler.lexer::lex)))
        ((eq r nil))
      (print r))))
