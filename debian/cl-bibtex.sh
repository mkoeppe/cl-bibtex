#! /bin/sh
export LISPTYPE=cmucl
/usr/share/cl-bibtex/run-lisp -i "/usr/share/common-lisp/source/bibtex/bibtex-program.lisp"  -x "(emulate-bibtex ARGV)" $@
