;;; A BibTeX re-implementation in Common Lisp
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(defpackage :kpathsea
  (:use :common-lisp)
  (:export "FIND-FILE"))

(load "kpathsea" :if-source-newer :compile)

(defpackage :bibtex-runtime
  (:use :common-lisp)
  (:export "*BIB-MACROS*" "*BIB-DATABASE*" "*BIB-ENTRIES*"
	   "*BIB-ENTRY*" "*BIB-PREAMBLE*" "*BIB-STYLE*"
	   "*BIB-FILES*" "*CITE-ALL-ENTRIES*" "*CITE-KEYS*"
	   "*BIB-ENTRY-TYPE-FUNCTIONS*"
	   "READ-AUX-FILE" "READ-BIB-DATABASE" "CITED-BIB-ENTRIES"
	   "READ-ALL-BIB-FILES-AND-COMPUTE-BIB-ENTRIES"
	   "BIBTEX-NAME" "MAKE-BIBTEX-NAME" "BIBTEX-NAME-FIRST"
	   "BIBTEX-NAME-VON" "BIBTEX-NAME-LAST" "BIBTEX-NAME-JR"
	   "*BIBTEX-SPLIT-INITIALS*" "PARSE-BIBTEX-NAME" "PARSE-BIBTEX-NAME-LIST"
	   "*BIBTEX-LONG-TOKEN-LENGTH*" "*BIBTEX-LONG-NAME-LENGTH*"
	   "FORMAT-BIBTEX-NAME"
	   "FORMAT-NTH-BIBTEX-NAME" "NUM-BIBTEX-NAMES" "BIBTEX-SUBSTRING"
	   "ADD-PERIOD-UNLESS-SENTENCE-END"
	   "WHITESPACE-P" "EMPTY-FIELD-P" "BIBTEX-STRING-PURIFY"
	   "BIBTEX-STRING-DOWNCASE" "BIBTEX-STRING-UPCASE" "BIBTEX-STRING-TITLEDOWNCASE"
	   "BIBTEX-STRING-PREFIX"
	   "BIB-WARN" "BIB-WARN*" "BIB-ERRROR" "BIB-FATAL"
	   #:*err-count* #:*history*
	   #:+spotless-history+ #:+warning-message+ #:+error-message+ #:+fatal-message+
	   #:*bbl-output*))
  
(load "bibtex-runtime" :if-source-newer :compile)

(defpackage :bibtex-compiler
  (:use :common-lisp :bibtex-runtime)
  (:export "COMPILE-BST-FILE" "BIBTEX" "CL-BIBTEX"))

(load "lisp-form-builder" :if-source-newer :compile)
(load "bst-functions" :if-source-newer :compile)
(load "interpreter" :if-source-newer :compile)
(load "bibtex-compiler" :if-source-newer :compile)
(load "bst-reader" :if-source-newer :compile)
(load "bst-builtins" :if-source-newer :compile)
(load "bibtex" :if-source-newer :compile)

