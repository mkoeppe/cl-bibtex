VERSION=1.0.1

SOURCES=Makefile AUTHORS COPYING COPYING-preamble.txt \
	ChangeLog README TODO NEWS \
	bibtex.asd bibtex.system \
	bibtex-compiler.lisp bibtex-runtime.lisp bibtex.lisp \
	bst-builtins.lisp bst-functions.lisp bst-reader.lisp \
	interpreter.lisp kpathsea.lisp lisp-form-builder.lisp \
	packages.lisp \
	test.lisp \
	bibtex run-lisp bibtex-program.lisp

tar:
	-rm -rf cl-bibtex-$(VERSION)
	mkdir cl-bibtex-$(VERSION)
	ln $(SOURCES) cl-bibtex-$(VERSION)
	tar cf cl-bibtex-$(VERSION).tar cl-bibtex-$(VERSION)
	gzip cl-bibtex-$(VERSION).tar
	-rm -rf cl-bibtex-$(VERSION)
