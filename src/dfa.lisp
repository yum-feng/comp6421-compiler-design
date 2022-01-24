(in-package :comp6421-compiler-design.dfa)

(defvar *keywords* '("if" "public" "read"
                     "then" "private" "write"
                     "else" "func" "return"
                     "integer" "var" "self"
                     "float" "struct" "inherits"
                     "void" "while" "let"
                     "func" "impl"))

(defmethod cl-dot:graph-object-node ((graph (eql 'edge-example)) object)
  (make-instance 'cl-dot:node
                 :attributes (list :label (format nil "Node ~A" object))))

(defmethod cl-dot:graph-object-edges ((graph (eql 'edge-example)))
  #((a b (:color :red    :style :dashed))
    (b c (:color :blue   :style :dotted))
    (c a (:color :yellow :style :bold))))

(defun c ()
  (let ((dgraph (cl-dot:generate-graph-from-roots 'edge-example '()
                                                  '(:rankdir "LR"))))
    (cl-dot:dot-graph dgraph "dfa.png" :format :png)))
