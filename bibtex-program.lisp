(in-package :common-lisp-user)

(let ((*load-verbose* nil)
      (*compile-verbose* nil))
  (load "/home/mkoeppe/p/cl-bibtex/bibtex.system")
  (mk:operate-on-system :bibtex 'compile)
  (mk:operate-on-system :bibtex 'load))

;;; The emulation of the bibtex program

(defun do-emulate-bibtex (command-line-args)
  (let ((*min-crossrefs* 2)
	(*bibtex-split-initials* nil)
	(file-stem nil))
    (dolist (arg command-line-args)
      (cond
       ((eql (mismatch "-min-crossrefs=" arg)
	     (length "-min-crossrefs="))
	(setq *min-crossrefs*
	      (parse-integer arg :start (length "-min-crossrefs="))))
       ((string= arg "-split-initials")
	(setq *bibtex-split-initials* t))
       ((string= arg "-terse")
	nil)
       ((and (not (string= arg ""))
	     (char= (char arg 0) #\-))
	(error "Unknown command-line switch: `~A'" arg))
       (t
	(if file-stem
	    (error "Need exactly one file argument; `~A' is extraneous" arg))
	(setq file-stem arg))))
    (unless file-stem
      (error "Need exactly one file argument"))
    (bibtex-compiler:bibtex file-stem)))

(defun emulate-bibtex (argv)
  ;;(princ "bar") (terpri)
  (let ((*gc-verbose* nil))
    (handler-case (do-emulate-bibtex argv)
      (error (condition)
	(format *error-output* "~&bibtex: ~A~%"
		condition)
	(unix:unix-exit 1)))))

