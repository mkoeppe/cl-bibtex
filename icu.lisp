;; A BibTeX re-implementation in Common Lisp 

;; File contributed by Paul Foley
;; Adds Unicode support in CMUCL

(in-package "CL-USER")

(defpackage "ICU")
(in-package "ICU")

(export '(ucol-open ucol-get-sort-key with-open-collator))

(eval-when (:compile-toplevel :execute)
  (alien:load-foreign "/usr/lib/libicui18n.so.38"
		      :libraries nil)
  (defconstant +ucol-open+ "ucol_open_3_8")
  (defconstant +ucol-close+ "ucol_close_3_8")
  (defconstant +ucol-get-sort-key+ "ucol_getSortKey_3_8"))

(alien:def-alien-routine (#.+ucol-open+ %ucol-open) sys:system-area-pointer
  (locale c-call:c-string)
  (status c-call:int :out))

(alien:def-alien-routine (#.+ucol-close+ ucol-close) c-call:void
  (collator sys:system-area-pointer))

(alien:def-alien-routine (#.+ucol-get-sort-key+ %ucol-get-sort-key) c-call:int
  (collator sys:system-area-pointer)
  (source sys:system-area-pointer)
  (slength c-call:int)
  (keyarr sys:system-area-pointer)
  (klength c-call:int))

(defun ucol-open (locale)
  (multiple-value-bind (collator status) (%ucol-open locale)
    ;; status < 0 : warning     (see /usr/include/unicode/utypes.h)
    ;; status = 0 : no problem
    ;; status > 0 : error
    (cond ((minusp status)		; warning
	   #| ignore |#)
	  ((plusp status)		; error
	   (error "Failed to create the collator (err=~D)" status)))
    collator))

(defun ucol-get-sort-key (collator string)
  (let* ((buffer (make-array (* 8 (length string))  ; should be enough
			     :element-type '(unsigned-byte 8)))
	 (size (%ucol-get-sort-key collator
				   (lisp:buffer-sap string)
				   -1
				   (lisp:buffer-sap buffer)
				   (length buffer))))
    (lisp::shrink-vector buffer size)))

(defmacro with-open-collator ((var locale) &body body)
  (let ((xvar (gensym)))
    `(let ((,xvar (ucol-open ,locale)))
      (unwind-protect (let ((,var ,xvar)) ,@body)
	(ucol-close ,xvar)))))
