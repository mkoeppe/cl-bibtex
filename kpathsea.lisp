;;; An interface to Kpathsea (TeX's file search library)
;;; Copr. 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(in-package :kpathsea)

;; We use CLOCC PORT to run programs if we are not running in CMUCL or SBCL.
#-(or cmu sbcl)
(require "PORT")

(defun find-file (name)
  #+cmu
  (let ((process
	 (extensions:run-program "kpsewhich" (list (namestring name))
				 :output :stream)))
    (let ((line (read-line (extensions:process-output process) nil nil)))
      (prog1
	  (and line
	       (parse-namestring line))
      (extensions:process-close process))))
  #+sbcl
  (let ((process
	 (sb-ext:run-program "/usr/bin/env"
			     (list "kpsewhich" (namestring name))
			     :output :stream)))
    (let ((line (read-line (sb-ext:process-output process) nil nil)))
      (prog1
	  (and line
	       (parse-namestring line))
      (sb-ext:process-close process))))
  #-(or cmu sbcl)
  (let ((stream (port:pipe-input "kpsewhich" (namestring name))))
    (let ((line (read-line stream nil nil)))
      (prog1
	  (and line (parse-namestring line))
	(port:close-pipe stream)))))


#|
(find-file "amsalpha.bst")
|#
