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
          #\0)
        (loop for r = (peek)
              while (and r (digit-p r))
              collect (next) into integer
              finally (return (coerce integer 'string))))))

(defun lex-fraction ()
  "return a fraction string."
    (loop for r = (peek)
          while (and r (digit-p r))
          collect (next) into fraction
          finally (return (coerce fraction 'string))))

(defun lex-exponent ()
  (let ((r (peek)))
    (if (eql r #\-)
        (progn
          (next)
          (cons #'\- (lex-integer)))
        (lex-integer))))

(defun lex-float (integer-part)
  "return a float token."
  (let* ((fraction-part (lex-fraction)) ; TODO: will need to handle bad fractions.
         (exponent-part (if (eql (peek) #\e)
                            (progn (next)
                                   (lex-exponent)))))
    (make-token :type "float"
                :lexeme (format nil "~@[~a~].~@[~a~]~@[e~a~]" integer-part fraction-part (if exponent-part
                                                                                                exponent-part))
                :location *line*)))

(defun lex-integer-or-float ()
  "return an integer or float token."
  (let ((integer-part (lex-integer))) ;; https://math.stackexchange.com/a/444600
    (if (eql (peek) #\.)
        (progn (next)
               (lex-float integer-part))
        (make-token :type "integer"
                    :lexeme (format nil "~a" integer-part)
                    :location *line*))))

(defun lex-inline-comment (&optional s)
  "return comment string."
  (let ((r (peek))
        (l *line*))
    (loop for r = (peek)
          until (eql *line* (1+ l))
          collect (next) into comment
          finally (return (coerce comment 'string)))))

;((eql r #\*) (loop for r = (peek)
;                   until (and (eql r #\*)
;                              (eql (peek) #\/))
;                   collect (next) into comment
;                   finally (return (coerce comment 'string))))

;  (loop for r = (next)
;        while (eql r (eql r #\i))
;        collect r into text
;        finally (return (coerce text 'string)))

; (and r
;      (eql l #\/)
;      (eql r #\*))
;
; ((let ((r (peek)))
;     (loop for )
;     (if (and (last t) (eql r #\*))
;         (lext-block-comment t)
;         ))
;   t)

; (defparameter *d* (make-array 0
;                                      :element-type 'character
;                                      :fill-pointer 0
;                                       :adjustable t))

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
          ((eql r #\/) (let ((r (next)))
                         (cond ((eql (peek) #\/) (make-token :type "inlinecmt" :lexeme (lex-inline-comment) :location *line*))
;;                               ((eql (peek) #\*) (make-token :type "blockcomment" :lexeme "n/a" #|(lex-block-comment r)|# :location *compile-file-pathname*))
                               (t (make-token :type "div" :lexeme (next) :location *line*)))))
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
          ((symbol-p rune) (lex-operator-or-punctuation-or-comment)))))
