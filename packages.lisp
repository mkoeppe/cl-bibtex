;;; A BibTeX re-implementation in Common Lisp
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(defpackage :kpathsea
  (:use :common-lisp)
  (:export #:find-file))

(defpackage :bibtex-runtime
  (:use :common-lisp)
  #+allegro (:shadow #:variable)
  (:export #:*bib-macros* #:*bib-database* #:*bib-entries*
	   #:*bib-entry* #:*bib-preamble* #:*bib-style*
	   #:*bib-files* #:*cite-all-entries* #:*cite-keys*
	   #:*bib-entry-type-functions* #:*min-crossrefs*
	   #:read-aux-file #:read-bib-database #:cited-bib-entries
	   #:write-bib-entry
	   #:read-all-bib-files-and-compute-bib-entries
	   #:bibtex-name #:make-bibtex-name #:bibtex-name-first
	   #:bibtex-name-von #:bibtex-name-last #:bibtex-name-jr
	   #:*bibtex-split-initials* #:parse-bibtex-name #:parse-bibtex-name-list
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
	   #:*bbl-output*))

(defpackage :bibtex-compiler
  (:use :common-lisp :bibtex-runtime)
  #+allegro (:shadow #:variable)
  (:export #:compile-bst-file #:bibtex
	   #:*bibtex-styles* #:*allow-load-lisp-bibtex-style*
	   #:register-bibtex-style #:define-bibtex-style #:find-bibtex-style))
