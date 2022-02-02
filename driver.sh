sbcl \
 --noinform \
 --eval '(asdf:load-system :comp6421-compiler-design)' \
 --eval '(progn (compiler.driver:run) (sb-ext:quit))' $1
