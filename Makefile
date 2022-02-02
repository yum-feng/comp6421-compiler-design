LISP ?= sbcl

lexnegativegrading:
	@$(LISP) \
	--noinform \
	--eval '(asdf:load-system :comp6421-compiler-design)' \
	--eval '(progn (compiler.driver:run) (sb-ext:quit))' t/lexnegativegrading/lexnegativegrading.src

lexpositivegrading:
	@$(LISP) \
	--noinform \
	--eval '(asdf:load-system :comp6421-compiler-design)' \
	--eval '(progn (compiler.driver:run) (sb-ext:quit))' t/lexpositivegrading/lexpositivegrading.src

.PHONY: clean
clean:
	rm -rf src/*.fasl
	rm -rf t/*.fasl
