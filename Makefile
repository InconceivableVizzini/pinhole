LISP ?= sbcl

build:
	$(LISP) --load pinhole.asd \
		--eval '(ql:quickload :pinhole)' \
		--eval '(asdf:make :pinhole)' \
		--eval '(quit)'
