(in-package :compiler.driver)

(defun compiler.driver:run ()
  (let* ((compiler.lexer:*stream* (open (nth 1 sb-ext:*posix-argv*)))
         (outlextokens (open (format nil "~a.outlextokens" (nth 1 sb-ext:*posix-argv*)) :direction :output :if-does-not-exist :create :if-exists :overwrite))
         (outlexerrors (open (format nil "~a.outlexerrors" (nth 1 sb-ext:*posix-argv*)) :direction :output :if-does-not-exist :create :if-exists :overwrite)))
    (loop for token = (compiler.lexer:lex)
          until (eq token nil)
          if (or (equal (compiler.lexer::token-type token) "invalidchar") (equal (compiler.lexer::token-type token) "invalidnum"))
            do (format outlexerrors "[~a, ~a, ~a]~%" (compiler.lexer::token-type token) (compiler.lexer::token-lexeme token) (compiler.lexer::token-location token))
          else
            do (format outlextokens "[~a, ~a, ~a]~%" (compiler.lexer::token-type token) (compiler.lexer::token-lexeme token) (compiler.lexer::token-location token)))
    (close outlextokens)
    (close outlexerrors)))

(defun d ()
  (write "hi"))
