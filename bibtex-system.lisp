
(defpackage bibtex-compiler
  (:use "COMMON-LISP"))

(load "kpathsea" :if-source-newer :compile)
(load "bibtex-runtime" :if-source-newer :compile)
(load "bibtex" :if-source-newer :compile)
(load "bibtex-compiler" :if-source-newer :compile)
