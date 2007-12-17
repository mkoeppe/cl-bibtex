;; A BibTeX re-implementation in Common Lisp
;; Copyright 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of version 2.1 of the GNU Lesser 
;; General Public License as published by the Free Software 
;; Foundation or any later version, as clarified by the preamble 
;; found in COPYING-preamble.txt. This preamble is in the style
;; of the Franz Inc. preamble at http://opensource.franz.com/preamble.html
;; with names and copyright holders altered accordingly.

(defpackage :kpathsea
  (:use :common-lisp)
  (:export #:find-file))

(defpackage :bibtex-runtime
  (:use :common-lisp)
  (:shadow #:variable)
  (:export #:*bib-macros* #:*bib-database* #:*bib-entries*
	   #:*bib-entry* #:*bib-preamble* #:*bib-style*
	   #:*bib-files* #:*cite-all-entries* #:*cite-keys*
	   #:*bib-entry-type-functions* #:*min-crossrefs*
	   #:bib-entry-cite-key #:bib-entry-type
	   #:read-aux-file #:read-bib-database #:cited-bib-entries
	   #:write-bib-entry
	   #:read-all-bib-files-and-compute-bib-entries
	   #:bibtex-name #:make-bibtex-name #:bibtex-name-first
	   #:bibtex-name-von #:bibtex-name-last #:bibtex-name-jr
	   #:*bibtex-split-initials* #:*bibtex-split-initials-already-warned-hashtable*
	   #:parse-bibtex-name #:parse-bibtex-name-list
	   #:*bibtex-long-token-length* #:*bibtex-long-name-length*
	   #:format-bibtex-name
	   #:format-nth-bibtex-name #:num-bibtex-names #:bibtex-substring
	   #:add-period-unless-sentence-end
	   #:whitespace-p #:empty-field-p #:bibtex-string-purify
	   #:bibtex-string-downcase #:bibtex-string-upcase #:bibtex-string-titledowncase
	   #:bibtex-string-prefix #:bibtex-string-width
	   #:bib-warn #:bib-warn* #:bib-error #:bib-fatal
	   #:*err-count* #:*history*
	   #:+spotless-history+ #:+warning-message+ #:+error-message+ #:+fatal-message+
	   #:*bbl-output* #:*bbl-min-print-line* #:*bbl-max-print-line*
	   #:bbl-print #:bbl-terpri #:with-bbl-output))

(defpackage :bibtex-compiler
  (:use :common-lisp :bibtex-runtime)
  (:shadow #:variable)
  (:export #:compile-bst-file #:bibtex
	   #:*bibtex-styles* #:*allow-load-lisp-bibtex-style*
	   #:register-bibtex-style #:define-bibtex-style #:find-bibtex-style
	   #:lisp-bibtex-style #:interpreted-bibtex-style))

(defpackage :bibtex-program
  (:use :common-lisp :bibtex-runtime :bibtex-compiler)
  (:export #:do-emulate-bibtex #:emulate-bibtex))
