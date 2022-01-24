(defsystem :comp6421-compiler-design
  :depends-on (:cl-dot)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "dfa")
     (:file "lexer")))))
