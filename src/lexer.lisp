(in-package :compiler.lexer)

(defstruct token type lexeme location)

(defvar *stream* nil) ; redefined in a lexical scope.

(defvar *line* 1)
(defvar *char* 1)
(defvar *position* 1)

(defparameter *operators-alist* '((eq . "==")
                                  (plus . "+")
                                  (or . "|")
                                  (neq . "<>")
                                  (minus . "-")
                                  (and . "&")
                                  (lt . "<")
                                  (mult . "*")
                                  (not . "!")
                                  (gt . ">")
                                  (div . "/")
                                  (leq . "<=")
                                  (assign . "=")
                                  (geq . ">=")))

(defparameter *punctuations-alist* '((openpar . "(")
                              (semicolon . ";")
                              (closepar . ")")
                              (comma . ",")
                              (opencubr . "{")
                              (dot . ".")
                              (closecubr . "}")
                              (colon . ":")
                              (opensqbr . "[")
                              (coloncolon . "::")
                              (closesqbr . "]")
                              (arrow . "->")))

;(delete-duplicates (sort (mapcan (lambda (lexeme) (coerce lexeme 'list)) (append *operators-alist* *punctuations-alist*)) #'char-lessp))
(defparameter *symbols* '("!"
                          "&"
                          "("
                          ")"
                          "*"
                          "+"
                          ","
                          "-"
                          "->"
                          "."
                          "/"
                          ":"
                          ":"
                          ";"
                          "<"
                          "<"
                          "<"
                          "="
                          "="
                          ">"
                          "="
                          "["
                          "]"
                          "{"
                          "|"
                          "}"))

(defparameter *reserved-words* '("if"      "public"  "read"
                                 "then"    "private" "write"
                                 "else"    "func"    "return"
                                 "integer" "var"     "self"
                                 "float"   "struct"  "inherits"
                                 "void"    "while"   "let"
                                 "func"    "impl"))

(defun peek ()
  "return a rune without consuming the stream."
  (peek-char nil *stream* nil))

(defun next ()
  "retun a rune from the stream."
  (let ((r (read-char *stream* nil)))
    (cond ((eql r #\Newline) (progn
                               (incf *line*)
                               (setf *char* 1)))
          (t (progn (incf *char*)
                    (incf *position*))))
          r))

(defun letter-p (r)
  (alpha-char-p r))

(defun digit-p (r)
  "return rune if rune is a digit."
  (digit-char-p r))

(defun nonzero-p (r)
  "return rune if rune is nonzero."
  (if (and (digit-char-p r) (not (eql r #\0)))
      r))

(defun symbol-p (r)
  "return rune if rune is a symbol."
  (if (member r *symbols* :test 'string=)
      r))

(defun lex-integer ()
  "return an integer string."
  (let ((r (peek)))
    (if (eql r #\0)
        (progn
          (next)
          "0")
        (loop for r = (peek)
              while (and r (digit-p r))
              collect (next) into integer
              finally (return (coerce integer 'string))))))

(defun lex-fraction ()
  "return a fraction string."
  (next) ; TODO: dirty fix to consume the assumed prefix #\.
  (loop for r = (peek)
        while (and r (digit-p r))
        collect (next) into fraction
        finally (return (coerce (cons #\. fraction) 'string))))

(defun lex-exponent ()
  (next) ;TODO: dirty fix to consume the assumed prefix #\e
  (let ((r (peek)))
    (if (eql r #\-)
        (concatenate 'string (progn (next) "e-") (lex-integer))
        (concatenate 'string "e" (lex-integer)))))

(defun lex-integer-or-float ()
  "return an integer or float token."
  (let ((integer-part (lex-integer)))
    (if (eql (peek) #\.)
        (let* ((fraction-part (lex-fraction))
               (exponent-part (if (eql (peek) #\e) (lex-exponent))))
          (make-token :type (if (or (and exponent-part (not (digit-p (char exponent-part (1- (length exponent-part))))))
                                    (and (> (length fraction-part) 2)
                                         (eql (char fraction-part (- (length fraction-part) 1)) #\0))) "invalidnum" "float")
                      :lexeme (format nil "~@[~a~]~@[~a~]~@[~a~]" integer-part fraction-part exponent-part)
                      :location *line*))
        (make-token :type "integer"
                    :lexeme (format nil "~a" integer-part)
                    :location *line*))))

(defun lex-inline-comment ()
  "return inline comment string."
  (loop for r = (next)
        until (or (not r)
                  (eql r #\Newline)
                  (eql r #\Return))
        collect r into text
        finally (return (coerce text 'string))))

(defun lex-block-comment ()
  (loop for r = (next)
        until (or (and (eql r #\*)
                       (eql (peek) #\/))
                  (not r))
        if (and (eql r #\/)
                (eql (peek) #\*)) ; entering a nested comment.
          append (cons r (lex-block-comment)) into text
        else
          collect r into text
        finally (next) (return (append text '(#\* #\/)))))

(defun lex-operator-or-punctuation-or-comment ()
  "return either an operator or punctuation token."
  (let ((r (peek)))
    (cond ((eql r #\!) (make-token :type "not" :lexeme (next) :location *line*))
          ((eql r #\&) (make-token :type "and" :lexeme (next) :location *line*))
          ((eql r #\() (make-token :type "openpar" :lexeme (next) :location *line*))
          ((eql r #\)) (make-token :type "closepar" :lexeme (next) :location *line*))
          ((eql r #\*) (make-token :type "mult" :lexeme (next) :location *line*))
          ((eql r #\+) (make-token :type "plus" :lexeme (next) :location *line*))
          ((eql r #\-) (let ((r (next)))
                         (if (eql (peek) #\>)
                             (make-token :type "arrow" :lexeme (list r (next)) :location *line*)
                             (make-token :type "minus" :lexeme r :location *line*))))
          ((eql r #\.) (make-token :type "dot" :lexeme (next) :location *line*))
          ((eql r #\,) (make-token :type "comma" :lexeme (next) :location *line*))
          ((eql r #\/) (let ((r (next))
                             (line *line*))
                         (cond ((eql (peek) #\/)
                                (next)
                                (make-token :type "inlinecmt" :lexeme (concatenate 'string "//" (lex-inline-comment)) :location line))
                               ((eql (peek) #\*)
                                (next)
                                (make-token :type "blockcomment" :lexeme (concatenate 'string "/*" (lex-block-comment)) :location line))
                               (t (make-token :type "div" :lexeme r :location *line*)))))
          ((eql r #\:) (let ((r (next)))
                         (cond ((eql (peek) #\:)
                                (make-token :type "coloncolon" :lexeme (list r (next) #|TODO: may not be the best idea to rely on evaluation order, and not sure on using a list either|#) :location *line*))
                               (t
                                (make-token :type "colon" :lexeme r :location *line*)))))
          ((eql r #\;) (make-token :type "semicolon" :lexeme (next) :location *line*))
          ((eql r #\<) (let ((r (next)))
                         (cond ((eql (peek) #\=)
                                (make-token :type "leq" :lexeme (list r (next) #|TODO: may not be the best idea to rely on evaluation order, and not sure on using a list either|#) :location *line*))
                               ((eql (peek) #\>)
                                (make-token :type "noteq" :lexeme (list r (next) #|TODO: may not be the best idea to rely on evaluation order, and not sure on using a list either|#) :location *line*))
                               (t
                                (make-token :type "lt" :lexeme r  :location *line*)))))
          ((eql r #\=) (let ((r (next)))
                         (if (eql (peek) #\=)
                             (make-token :type "eq" :lexeme (list r (next) #|TODO: may not be the best idea to rely on evaluation order, and not sure on using a list either|#) :location *line*)
                             (make-token :type "assign" :lexeme r :location *line*))))
          ((eql r #\>) (let ((r (next)))
                         (cond ((eql (peek) #\=)
                                (make-token :type "geq" :lexeme (list r (next) #|TODO: may not be the best idea to rely on evaluation order, and not sure on using a list either|#) :location *line*))
                               (t
                                (make-token :type "gt" :lexeme r :location *line*)))))
          ((eql r #\[) (make-token :type "opensqbr" :lexeme (next) :location *line*))
          ((eql r #\]) (make-token :type "closesqbr" :lexeme (next) :location *line*))
          ((eql r #\{) (make-token :type "opencubr" :lexeme (next) :location *line*))
          ((eql r #\|) (make-token :type "or" :lexeme (next) :location *line*))
          ((eql r #\}) (make-token :type "closecubr" :lexeme (next) :location *line*)))))

(defun lex-reserved-words-or-id (&optional (prefix (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
  (let ((r (peek)))
    (if (or (letter-p r) (digit-p r) (eql r #\_))
        (progn (vector-push-extend (next) prefix)
               (lex-reserved-words-or-id prefix))
        (if (member prefix *reserved-words* :test 'equal)
            (make-token :type prefix
                        :lexeme prefix
                        :location *line*)
            (make-token :type "id"
                        :lexeme prefix
                        :location *line*)))))

(defun lex ()
  (loop for rune = (peek)
        while (or (eq rune #\Return)
                  (eq rune #\Newline)
                  (eq rune #\Tab)
                  (eq rune #\Space))
        do (next))
  (let ((rune (peek)))
    (cond ((not rune) nil)
          ((or (eq rune #\Newline)
               (eq rune #\Tab)
               (eq rune #\Space)) (progn (next)))
          ((letter-p rune) (lex-reserved-words-or-id))
          ((digit-p rune) (lex-integer-or-float))
          ((symbol-p rune) (lex-operator-or-punctuation-or-comment))
          (t (make-token :type "invalidchar"
                         :lexeme (next)
                         :location *line*)))))
