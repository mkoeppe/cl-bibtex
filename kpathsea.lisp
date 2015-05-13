;; An interface to Kpathsea (TeX's file search library)
;; Copyright 2001, 2002, 2007 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of version 2.1 of the GNU Lesser 
;; General Public License as published by the Free Software 
;; Foundation or any later version, as clarified by the preamble 
;; found in COPYING-preamble.txt. This preamble is in the style
;; of the Franz Inc. preamble at http://opensource.franz.com/preamble.html
;; with names and copyright holders altered accordingly.

(in-package :kpathsea)

;; We use CLOCC PORT to run programs if we are not running in CMUCL or SBCL.
;;#+clisp
;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (cl:require :PORT))

;; #-(or cmu sbcl clisp)
;; (cl:require :PORT)

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
  #+abcl
  (let ((process
	 (sys:run-program "/usr/bin/env"
			     (list "kpsewhich" (namestring name)))))
    (let ((line (read-line (sys:process-output process) nil nil)))
      (and line
           (parse-namestring line))))
  #+allegro
  (let ((stream (excl:run-shell-command (vector "/bin/sh"
						"/bin/sh"
						"-c"
						(format nil "~A ~A" "kpsewhich" (namestring name)))
					:output :stream :wait nil)
	  ;; Using run-shell-command with a vector is much faster
	  ;; than with a list (tries to run $SHELL! -- this is what PORT does)
	  ))
    (let ((line (read-line stream nil nil)))
      (prog1
	  (and line (parse-namestring line))
	(port:close-pipe stream))))
  #-(or cmu sbcl abcl allegro)
  (let ((stream (port:pipe-input "kpsewhich" (namestring name))))
    (let ((line (read-line stream nil nil)))
      (prog1
	  (and line (parse-namestring line))
	(port:close-pipe stream)))))


#|
(find-file "amsalpha.bst")
|#
