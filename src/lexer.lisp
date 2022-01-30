(in-package :compiler.lexer)

(defstruct token type lexeme location)

(defvar *line* 1)
(defvar *char* 1)
(defvar *position* 1)

(defparameter *operators* '("==" "+" "|"
                            "<>" "-" "&"
                            "<"  "*" "!"
                            ">"  "/"
                            "<=" "="
                            ">="))

(defparameter *punctuation* '("(" ";"
                              ")" ","
                              "{" "."
                              "}" ":"
                              "[" "::"
                              "]" "->"))

(defparameter *reserved-words* '("if"      "public"  "read"
                                 "then"    "private" "write"
                                 "else"    "func"    "return"
                                 "integer" "var"     "self"
                                 "float"   "struct"  "inherits"
                                 "void"    "while"   "let"
                                 "func"    "impl"))

(defparameter *stream* (open "../t/iter.sample")) ;; TODO: maybe a global variable is not the best.

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
  "return false if rune is not a digit."
  (digit-char-p r))

(defun nonzero-p (r)
  "return false if rune is not a digit."
  (if (and (digit-char-p r) (not (eql r #\0)))
      r))

(defun symbol-p (r)
  ;; TODO: find a better way to do this, and use sets.
  (let ((symbols (delete-duplicates (sort (mapcan (lambda (lexeme) (coerce lexeme 'list)) (append *operators* *punctuation*)) #'char-lessp))))
    (if (member r symbols)
        t)))

(defun lex-integer (&optional (sum 0))
  "return an integer."
  (let ((r (peek)))
    (if (and r (digit-p r))
        (cond ((eql r #\0) (digit-p (next)))
              (t (lex-integer (+ (* sum 10) (digit-p (next))))))
        sum)))

(defun lex-fraction (&optional (sum 0) (denominator 1))
  "return a fraction."
  (let ((r (peek))
        (denominator (* denominator 10)))
    (if (and r (digit-p r))
        (cond ((nonzero-p r) (lex-fraction (+ sum (/ (digit-p (next)) denominator)) denominator))
              ((eq r #\0) (let ((o (lex-fraction (+ sum (/ (digit-p (next)) denominator)) denominator)))
                            (if (eql sum o) ; if sum does not change, then there was a trailing of zeros until the end.
                                (write "bad." #|TODO: better error message |#)
                                (lex-fraction o denominator)))))
        sum)))

(defun lex-exponent ()
  (let ((r (peek)))
    (if (eql r #\-)
        (progn
          (next)
          (* -1 (lex-integer)))
        (lex-integer))))

(defun lex-float (integer-part)
  "return a float token."
  (let* ((fraction-part (lex-fraction))
         (exponent-part (lex-exponent))) ; TODO: will need to handle bad fractions.
    (make-token :type "float"
                :lexeme (format nil "~d.~de~d" integer-part fraction-part exponent-part) ; TODO: fraction is literally being displayed as a fraction, lol.
                :location *position*)))

(defun lex-integer-or-float ()
  "return an integer or float token."
  (let ((integer-part (lex-integer))) ;; https://math.stackexchange.com/a/444600
    (if (eql (peek) #\.)
        (progn (next)
               (lex-float integer-part))
        (make-token :type "integer"
                    :lexeme (format nil "~d" integer-part)
                    :location *position* #| TODO: needs to be line number |#))))

(defun lex-operator ()
  "return an operator token."
  (let ((r (peek)))
    (if (or ))
    )
  )


(defun lex-operator-or-punctuation (r)
  "return operator token."
  )

(defun lex ()
  (let ((rune (peek)))
    (cond ((member rune #|fix this shit|# '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\l #\m #\n #\o #\p #\r #\s #\t #\u #\v #\w #\A #\B #\C #\D #\E #\F #\G #\H #\I #\L #\M #\N #\O #\P #\R #\S #\T #\U #\V #\W)) (reserved-word-or-id))
          ((digit-p rune) (lex-integer-or-float))
          ((symbol-p rune) (operator-or-punctuation)))
    ))
