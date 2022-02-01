(defpackage :compiler.lexer
  (:use cl)
  (:export :lex)
  (:export :token :token-type :token-lexeme :token-location :*stream*))

(defpackage :compiler.driver
  (:use cl)
  (:use compiler.lexer)
  (:export :run :d))
