;;; Tests for CL-BibTeX


(defvar *tetex-bibliography-styles*
  '("amsalpha.bst"
    "amsplain.bst"
    "amsxport.bst"
    "abbrv.bst"
    "alpha.bst"
    "apalike.bst"
    "ieeetr.bst"
    "plain.bst"
    "siam.bst"
    "unsrt.bst"
    "gerabbrv.bst"
    "geralpha.bst"
    "gerapali.bst"
    "gerplain.bst"
    "gerunsrt.bst"
    "acm.bst"
    "abbrvnat.bst"
    "plainnat.bst"
    "unsrtnat.bst"
    "plabbrv.bst"
    "plalpha.bst"
    "plplain.bst"
    "plunsrt.bst"
    "apsrev.bst"
    "apsrmp.bst"))

(defvar *ctan-bibliography-styles*
  '("biblio/bibtex/contrib/aaai-named.bst"
    "biblio/bibtex/contrib/abstract.bst"
    "biblio/bibtex/contrib/abstyles-babel/aabbrv.bst"
    "biblio/bibtex/contrib/abstyles-babel/aalpha.bst"
    "biblio/bibtex/contrib/abstyles-babel/anotit.bst"
    "biblio/bibtex/contrib/abstyles-babel/aplain.bst"
    "biblio/bibtex/contrib/abstyles-babel/aunsnot.bst"
    "biblio/bibtex/contrib/abstyles-babel/aunsrt.bst"
    "biblio/bibtex/contrib/abstyles/aabbrv.bst"
    "biblio/bibtex/contrib/abstyles/aalpha.bst"
    "biblio/bibtex/contrib/abstyles/anotit.bst"
    "biblio/bibtex/contrib/abstyles/aplain.bst"
    "biblio/bibtex/contrib/abstyles/aunsnot.bst"
    "biblio/bibtex/contrib/abstyles/aunsrt.bst"
    ;;"biblio/bibtex/contrib/acmtrans/acmtrans.bst"
    ; contains broken code in format.thesis.type
    "biblio/bibtex/contrib/agsm.bst"
    "biblio/bibtex/contrib/alphanum.bst"
    "biblio/bibtex/contrib/ama.bst"
    "biblio/bibtex/contrib/annotate.bst"
    "biblio/bibtex/contrib/annotation.bst"
    "biblio/bibtex/contrib/apa.bst"
    "biblio/bibtex/contrib/apacite/apacite.bst"
    "biblio/bibtex/contrib/apalike.bst"
    "biblio/bibtex/contrib/apalike2.bst"
    "biblio/bibtex/contrib/apasoft.bst"
    "biblio/bibtex/contrib/astron/astron.bst"
    "biblio/bibtex/contrib/authordate1.bst"
    "biblio/bibtex/contrib/authordate2.bst"
    "biblio/bibtex/contrib/authordate3.bst"
    "biblio/bibtex/contrib/authordate4.bst"
    "biblio/bibtex/contrib/automatica.bst"
    "biblio/bibtex/contrib/bbs.bst"
    "biblio/bibtex/contrib/bib-fr/abbrv-fr.bst"
    "biblio/bibtex/contrib/bib-fr/alpha-fr.bst"
    "biblio/bibtex/contrib/bib-fr/plain-fr.bst"
    "biblio/bibtex/contrib/bib-fr/unsrt-fr.bst"
    "biblio/bibtex/contrib/bibhtml/alphahtml.bst"
    "biblio/bibtex/contrib/bibhtml/alphahtmldate.bst"
    "biblio/bibtex/contrib/bibhtml/alphahtmldater.bst"
    "biblio/bibtex/contrib/bibhtml/plainhtml.bst"
    "biblio/bibtex/contrib/bibhtml/plainhtmldate.bst"
    "biblio/bibtex/contrib/bibhtml/plainhtmldater.bst"
    "biblio/bibtex/contrib/cbe.bst"
    ;; "biblio/bibtex/contrib/cell.bst" ; broken
    "biblio/bibtex/contrib/chem-journal/jcc.bst"
    "biblio/bibtex/contrib/chem-journal/jpc.bst"
    "biblio/bibtex/contrib/chem-journal/pccp.bst"
    "biblio/bibtex/contrib/chem-journal/revcompchem.bst"
    "biblio/bibtex/contrib/chicago.bst"
    "biblio/bibtex/contrib/chicagoa.bst"
    "biblio/bibtex/contrib/cj.bst"
    "biblio/bibtex/contrib/dcu.bst"
    "biblio/bibtex/contrib/decsci.bst"
    ;; "biblio/bibtex/contrib/directory/address-html.bst" ; broken in format.years
    "biblio/bibtex/contrib/directory/address.bst"
    "biblio/bibtex/contrib/directory/birthday.bst"
    "biblio/bibtex/contrib/directory/email-html.bst"
    "biblio/bibtex/contrib/directory/email.bst"
    "biblio/bibtex/contrib/directory/letter.bst"
    "biblio/bibtex/contrib/directory/phone.bst"
    "biblio/bibtex/contrib/dk-bib/dk-abbrv.bst"
    "biblio/bibtex/contrib/dk-bib/dk-alpha.bst"
    "biblio/bibtex/contrib/dk-bib/dk-plain.bst"
    "biblio/bibtex/contrib/dk-bib/dk-unsrt.bst"
    "biblio/bibtex/contrib/economic/aer.bst"
    "biblio/bibtex/contrib/economic/aertt.bst"
    "biblio/bibtex/contrib/economic/agsm.bst"
    "biblio/bibtex/contrib/economic/cje.bst"
    "biblio/bibtex/contrib/economic/econometrica.bst"
    "biblio/bibtex/contrib/economic/ecta.bst"
    "biblio/bibtex/contrib/economic/pnas.bst"
    "biblio/bibtex/contrib/elsevier/elsart-harv.bst"
    "biblio/bibtex/contrib/elsevier/elsart-num.bst"
    "biblio/bibtex/contrib/finplain.bst"
    "biblio/bibtex/contrib/geralpha/galphac.bst"
    ;; "biblio/bibtex/contrib/geralpha/geralpha.bst" ; leading garbage
    "biblio/bibtex/contrib/german/din1505/abbrvdin.bst"
    "biblio/bibtex/contrib/german/din1505/alphadin.bst"
    ;; "biblio/bibtex/contrib/german/din1505/natdin.bst"
					; broken in misc
    "biblio/bibtex/contrib/german/din1505/plaindin.bst"
    "biblio/bibtex/contrib/german/din1505/unsrtdin.bst"
    "biblio/bibtex/contrib/german/dinat/dinat.bst"
    "biblio/bibtex/contrib/germbib/bst/gerabbrv.bst"
    "biblio/bibtex/contrib/germbib/bst/geralpha.bst"
    "biblio/bibtex/contrib/germbib/bst/gerapali.bst"
    "biblio/bibtex/contrib/germbib/bst/gerplain.bst"
    "biblio/bibtex/contrib/germbib/bst/gerunsrt.bst"
    "biblio/bibtex/contrib/getrefs/getrefs.bst"
    "biblio/bibtex/contrib/humanbio.bst"
    "biblio/bibtex/contrib/humannat.bst"
    "biblio/bibtex/contrib/inlinebib/inlinebib.bst"
    "biblio/bibtex/contrib/is-abbrv.bst"
    "biblio/bibtex/contrib/is-alpha.bst"
    "biblio/bibtex/contrib/is-plain.bst"
    "biblio/bibtex/contrib/is-unsrt.bst"
    "biblio/bibtex/contrib/jas99.bst"
    "biblio/bibtex/contrib/jas99_m.bst"
    "biblio/bibtex/contrib/jbact.bst"
    "biblio/bibtex/contrib/jmb.bst"
    "biblio/bibtex/contrib/jqt1999.bst"
    "biblio/bibtex/contrib/jtb.bst"
    "biblio/bibtex/contrib/jtbnew.bst"
    "biblio/bibtex/contrib/kluwer.bst"
    "biblio/bibtex/contrib/mla/hum2.bst"
    "biblio/bibtex/contrib/mla/mla.bst"
    "biblio/bibtex/contrib/mla/mlaa.bst"
    "biblio/bibtex/contrib/named.bst"
    "biblio/bibtex/contrib/namunsrt.bst"
    "biblio/bibtex/contrib/nar.bst"
    "biblio/bibtex/contrib/nature.bst"
    "biblio/bibtex/contrib/neuron.bst"
    "biblio/bibtex/contrib/newapa.bst"
    "biblio/bibtex/contrib/norbib/norabbrv.bst"
    "biblio/bibtex/contrib/norbib/noralpha.bst"
    "biblio/bibtex/contrib/norbib/norplain.bst"
    "biblio/bibtex/contrib/norbib/norunsrt.bst"
    "biblio/bibtex/contrib/osa.bst"
    "biblio/bibtex/contrib/oxford/oxford_en.bst"
    "biblio/bibtex/contrib/oxford/oxford_in.bst"
    "biblio/bibtex/contrib/oxford/oxford_se.bst"
    "biblio/bibtex/contrib/phjcp.bst"
    "biblio/bibtex/contrib/phy-bstyles/aip.bst"
    "biblio/bibtex/contrib/phy-bstyles/cpc.bst"
    "biblio/bibtex/contrib/phy-bstyles/iaea.bst"
    "biblio/bibtex/contrib/phy-bstyles/jcp.bst"
    "biblio/bibtex/contrib/phy-bstyles/nf.bst"
    "biblio/bibtex/contrib/phy-bstyles/nflet.bst"
    "biblio/bibtex/contrib/phy-bstyles/pf.bst"
    "biblio/bibtex/contrib/phy-bstyles/ppcf.bst"
    "biblio/bibtex/contrib/phy-bstyles/report.bst"
    "biblio/bibtex/contrib/phy-bstyles/rmp.bst"
    "biblio/bibtex/contrib/plainyr.bst"
    "biblio/bibtex/contrib/refer.bst"
    "biblio/bibtex/contrib/swebib/sweabbrv.bst"
    "biblio/bibtex/contrib/swebib/swealpha.bst"
    "biblio/bibtex/contrib/swebib/sweplain.bst"
    "biblio/bibtex/contrib/these.bst"
    "biblio/bibtex/contrib/urlbst/abbrvurl.bst"
    "biblio/bibtex/contrib/urlbst/alphaurl.bst"
    "biblio/bibtex/contrib/urlbst/plainurl.bst"
    "biblio/bibtex/contrib/urlbst/unsrturl.bst"
    "biblio/bibtex/contrib/wmaainf.bst"
    "biblio/bibtex/distribs/styles/abbrv.bst"
    "biblio/bibtex/distribs/styles/alpha.bst"
    "biblio/bibtex/distribs/styles/apalike.bst"
    "biblio/bibtex/distribs/styles/ieeetr.bst"
    "biblio/bibtex/distribs/styles/plain.bst"
    "biblio/bibtex/distribs/styles/siam.bst"
    "biblio/bibtex/distribs/styles/unsrt.bst"
    "biblio/bibtex/utils/bibtools/abstract.bst"
    "biblio/bibtex/utils/bibtools/citekeys.bst"
    "biblio/bibtex/utils/bibtools/html-alpha.bst"
    "biblio/bibtex/utils/bibtools/html-long.bst"
    "biblio/bibtex/utils/bibtools/html-longp.bst"
    "biblio/bibtex/utils/bibtools/html-short.bst"
    "biblio/bibtex/utils/bibtools/subset-dfk.bst"
    "biblio/bibtex/utils/bibtools/subset-nocomment.bst"
    "biblio/bibtex/utils/bibtools/subset.bst"
    "biblio/bibtex/utils/noTeX.bst"
    ;;"biblio/bibtex/utils/refer-tools/refer.bst"  ; contains trailing garbage
    "fonts/greek/package-babel/BibTeX/hellas.bst"
    ;;"graphics/metapost/macros/3d/doc/ltugbib.bst"
	       	; maybe.other.name.field is a higher-order function
    "info/mil3/ams-alph.bst"
    "info/mil3/ams-pln.bst"
    "info/simplified-latex/plain-annote.bst"
    "language/french/contrib/fralpha.bst"
    "language/french/contrib/frapalike.bst"
    "language/french/contrib/frplain.bst"
    "language/korean/HLaTeX/contrib/halpha.bst"
    "macros/latex/contrib/other/asaetr/asaetr.bst"
    "macros/latex/contrib/other/imac/imac.bst"
    "macros/latex/contrib/other/siam/siamproc.bst"
    "macros/latex/contrib/supported/IEEEtran/bibtex/IEEEtran.bst"
    "macros/latex/contrib/supported/IEEEtran/bibtex/IEEEtranS.bst"
    "macros/latex/contrib/supported/adfathesis/adfathesis.bst"
    "macros/latex/contrib/supported/afthesis/thesnumb.bst"
    "macros/latex/contrib/supported/aguplus/agu.bst"
    "macros/latex/contrib/supported/aguplus/agufull.bst"
    "macros/latex/contrib/supported/ascelike/ascelike.bst"
    "macros/latex/contrib/supported/frankenstein/achicago.bst"
    "macros/latex/contrib/supported/gatech-thesis/gatech-thesis-losa.bst"
    "macros/latex/contrib/supported/gatech-thesis/gatech-thesis.bst"
    "macros/latex/contrib/supported/gloss/glsplain.bst"
    "macros/latex/contrib/supported/gloss/glsshort.bst"
    "macros/latex/contrib/supported/harvard/agsm.bst"
    "macros/latex/contrib/supported/harvard/apsr.bst"
    "macros/latex/contrib/supported/harvard/dcu.bst"
    "macros/latex/contrib/supported/harvard/jmr.bst"
    "macros/latex/contrib/supported/harvard/jphysicsB.bst"
    "macros/latex/contrib/supported/harvard/kluwer.bst"
    "macros/latex/contrib/supported/harvard/nederlands.bst"
    "macros/latex/contrib/supported/hc/hc-de.bst"
    "macros/latex/contrib/supported/hc/hc-en.bst"
    "macros/latex/contrib/supported/ieeepes/ieeepes.bst"
    "macros/latex/contrib/supported/jkthesis/jkthesis.bst"
    "macros/latex/contrib/supported/kluwer/klunamed.bst"
    "macros/latex/contrib/supported/kluwer/klunum.bst"
    "macros/latex/contrib/supported/koma-script/scrguide/english/scrguide-en.bst"
    "macros/latex/contrib/supported/koma-script/scrguide/scrguide.bst"
    "macros/latex/contrib/supported/mlbib/journal.bst"
    "macros/latex/contrib/supported/mlbib/paper.bst"
    "macros/latex/contrib/supported/mlbib/thesis.bst"
    "macros/latex/contrib/supported/mslapa/mslapa.bst"
    "macros/latex/contrib/supported/multibib/mbplain.bst"
    "macros/latex/contrib/supported/natbib/abbrvnat.bst"
    "macros/latex/contrib/supported/natbib/plainnat.bst"
    "macros/latex/contrib/supported/natbib/unsrtnat.bst"
    "macros/latex/contrib/supported/revtex/apsrev.bst"
    "macros/latex/contrib/supported/revtex/apsrmp.bst"
    "macros/latex/contrib/supported/smflatex/smfalpha.bst"
    "macros/latex/contrib/supported/smflatex/smfplain.bst"
    "macros/latex/contrib/supported/spie/spiebib.bst"
    "macros/latex/required/amslatex/classes/amsalpha.bst"
    "macros/latex/required/amslatex/classes/amsplain.bst"
    "macros/latex209/contrib/acm/acm.bst"
    "macros/latex209/contrib/acs/acs.bst"
    "macros/latex209/contrib/aguplus/agu.bst"
    "macros/latex209/contrib/ifac/IFAC.bst"
    "macros/latex209/contrib/ijc/ijc.bst"
    "macros/latex209/contrib/kluwer/kluwer.bst"
    "macros/latex209/contrib/ml/plainml.bst"
    "macros/latex209/contrib/newapa/newapa.bst"
    "macros/latex209/contrib/siam/siam.bst"
    "macros/latex209/contrib/systcontrolletters/scl.bst"
    "macros/latex209/contrib/theapa/theapa.bst"
    "macros/texsis/bibtex/texsis.bst"
    "nonfree/language/french/frenchpro/french/contrib/french_bst/fracm.bst"
    "nonfree/language/french/frenchpro/french/contrib/french_bst/frplainnat.bst"
    "nonfree/macros/latex/contrib/supported/ifacmtg/ifac.bst"
    "nonfree/macros/latex/contrib/supported/mnras/mn2e.bst"
    "obsolete/biblio/bibtex/contrib/acm.bst"
    ;; "obsolete/biblio/bibtex/contrib/cea.bst" ; trailing garbage
    "obsolete/biblio/bibtex/contrib/phaip.bst"
    "obsolete/biblio/bibtex/contrib/phapalik.bst"
    "obsolete/biblio/bibtex/contrib/phcpc.bst"
    "obsolete/biblio/bibtex/contrib/phiaea.bst"
    "obsolete/biblio/bibtex/contrib/phnf.bst"
    "obsolete/biblio/bibtex/contrib/phnflet.bst"
    "obsolete/biblio/bibtex/contrib/phpf.bst"
    "obsolete/biblio/bibtex/contrib/phppcf.bst"
    "obsolete/biblio/bibtex/contrib/phreport.bst"
    "obsolete/biblio/bibtex/contrib/phrmp.bst"
    ;; "obsolete/biblio/bibtex/contrib/physics.bst"
    ; this must be run through the C preprocessor to produce BST files
    "usergrps/dante/dtk/unpacked/dtk.bst"
    "usergrps/uktug/baskervi/6_6/tex-live.bst"
    "web/noweb/src/xdoc/chicago.bst"))

(defvar *ctan-directory* "/home/mkoeppe/p/cl-bibtex/ctan/")

(defvar *bib-style-files*
  (nconc
   (loop for style in *tetex-bibliography-styles*
	 for style-file-name = (kpathsea:find-file style)
	 when style-file-name
	 collect style-file-name)
   (loop for style in *ctan-bibliography-styles*
	 collect (merge-pathnames style *ctan-directory*))))

(defvar *test-bibliographies* '("/home/mkoeppe/p/cl-bibtex/examples/iba-bib"))

;; Running the tests

(defvar *temp-directory* "/home/mkoeppe/p/cl-bibtex/tmp/")

(defmacro with-temporary-aux-file ((aux-file
				    &key citations bibdata bibstyle)
				   &body body)
  `(progn (with-open-file (f ,aux-file :direction :output
			   :if-exists :supersede)
	    (dolist (citation ,citations)
	      (format f "\\citation{~A}~%" citation))
	    (dolist (bibdatum ,bibdata)
	      (format f "\\bibdata{~A}~%" bibdatum))
	    (format f "\\bibstyle{~A}~%" ,bibstyle))
    ,@body))

(defun test-style-file (style-file-name)
  (let ((conditions '()))
    (handler-case 
	(handler-bind ((condition
			(lambda (condition)
			  (push condition conditions))))
	  (let ((lisp-file-name (make-pathname :type "lbst" 
					       :directory *temp-directory*
					       :defaults style-file-name)))
	    (handler-case
		(progn
		  (format *error-output* "~&~%***** Compiling style ~A *****~%"
			  style-file-name)
		  (bibtex-compiler:compile-bst-file style-file-name lisp-file-name)
		  (format *error-output* "~&----- Compiling Lisp program ~A~%"
			  lisp-file-name)
		  (multiple-value-bind (output-truename warnings-p failure-p)
		      (compile-file lisp-file-name :print nil)
		    (declare (ignore warnings-p))
		    (unless failure-p
		      (format *error-output* "~&----- Loading compiled Lisp program ~A~%"
			      output-truename)
		      (load output-truename)
		      (format *error-output* "~&----- Running Lisp program ~A~%"
			      output-truename)
		      (let ((aux-file (make-pathname :type "aux"
						     :directory *temp-directory*
						     :defaults style-file-name))
			    (cl-bbl-file (make-pathname :type "clbbl"
							:directory *temp-directory*
							:defaults style-file-name))
			    (bbl-file (make-pathname :type "bbl"
						     :directory *temp-directory*
						     :defaults style-file-name))
			    (diff-file (make-pathname :type "bbldiff"
						      :directory *temp-directory*
						      :defaults style-file-name)))
			(with-temporary-aux-file
			    (aux-file :citations '("*")
				      :bibdata *test-bibliographies*
				      :bibstyle (namestring
						 (make-pathname :type nil
								:defaults style-file-name)))
			  (bibtex-compiler:bibtex (namestring aux-file))
			  (rename-file bbl-file cl-bbl-file)
			  (format *error-output* "~&----- Running the original BibTeX~%")
			  (unless (zerop (call-original-bibtex aux-file))
			    (error "Original BibTeX failed."))
			  (format *error-output* "~&----- Comparing results~%")
			  (if (zerop (call-diff bbl-file cl-bbl-file diff-file))
			      (format *error-output* "Identical.~%")
			      (error "BBL files differ, diff in ~A"
				     diff-file))))))))))
      (error (condition)
	(princ condition)
	(terpri)))
    (nreverse conditions)))

(defvar *style-files/results* '())

(defun test-remaining-style-files ()
  (dolist (style-file-name
	    (set-difference *bib-style-files*
			    (mapcar #'car *style-files/results*)
			    :test #'equal))
    (push (list style-file-name
		(test-style-file style-file-name))
	  *style-files/results*))
  (show-results))

(defun clear-results ()
  (setq *style-files/results* '()))

(defun show-results ()
  (format t "Style files:~%~:{~&~A~%~{~4T~W~%~}~}~%" *style-files/results*))

(defvar *original-bibtex* "/usr/bin/bibtex.tetex")

(defun call-original-bibtex (file-stem)
  (let ((process
	 (extensions:run-program *original-bibtex*
				 (list (namestring
					(make-pathname :type nil
						       :defaults file-stem)))
				 :output t)))
    (prog1 (process-exit-code process)
      (process-close process))))

(defun call-diff (file-a file-b &optional (output t))
  (let ((process 
	 (extensions:run-program "/usr/bin/diff"
				 `("-u" ,(namestring file-a) ,(namestring file-b))
				 :output output
				 :if-output-exists :supersede)))
    (prog1 (process-exit-code process)
      (process-close process))))
				 