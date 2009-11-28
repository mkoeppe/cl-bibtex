;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CMP -*-

;;; $Revision: 1.2 $
;;; Copyright © 2005 Paul Foley (mycroft@actrix.gen.nz)
;;; All rights reserved.  Use and verbatim redistribution permitted.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.
#+CMU (ext:file-comment "$Header: /sources/cl-bibtex/cl-bibtex/cmp.lisp,v 1.2 2009/11/28 03:09:51 mkoeppe Exp $")

(defpackage "CMP"
  (:use "COMMON-LISP")
  (:export "CMP" "HASH" "CMP<" "CMP>" "CMP=" "CMP<=" "CMP>=" "CMP/="))

(in-package "CMP")

(declaim (ftype (function (t t) (member -1 0 +1 nil)) cmp)
	 (ftype (function (t) (integer 0 #.most-positive-fixnum)) hash))

(defgeneric cmp (a b)
  (:documentation
   "Compare A and B; return -1 if A<B, 0 if A=B, +1 if A>B.  May return NIL
if A and B are not equal and have no meaningful order relation.")
  (:method ((a t) (b t))
    (if (equal a b) 0 nil))
  (:method ((a real) (b real))
    (cond ((< a b) -1) ((> a b) +1) (t 0)))
  (:method ((a character) (b character))
    (cond ((char< a b) -1) ((char> a b) +1) (t 0)))
  (:method ((a string) (b string))
    (cond ((string< a b) -1) ((string> a b) +1) (t 0)))
  #| more "builtin" methods? |#)

(defgeneric hash (thing)
  (:documentation "Hash such that (cmp= A B) => (= (hash A) (hash B))")
  (:method ((thing t)) (sxhash thing)))

(declaim (inline two-arg-cmp<  two-arg-cmp>  two-arg-cmp=
		 two-arg-cmp<= two-arg-cmp>= two-arg-cmp/=))

(defun two-arg-cmp< (a b)
  (and (not (eq a b)) (< (or (cmp a b) +1) 0)))

(defun two-arg-cmp> (a b)
  (and (not (eq a b)) (> (or (cmp a b) -1) 0)))

(defun two-arg-cmp= (a b)
  (or (eq a b) (= (or (cmp a b) 1) 0)))

(defun two-arg-cmp<= (a b)
  (or (eq a b) (<= (or (cmp a b) +1) 0)))

(defun two-arg-cmp>= (a b)
  (or (eq a b) (>= (or (cmp a b) -1) 0)))

(defun two-arg-cmp/= (a b)
  (and (not (eq a b)) (/= (or (cmp a b) 1) 0)))


(macrolet ((frob (name two-arg docstring)
	     `(progn
	        (declaim (ftype (function (t &rest t) boolean) ,name)
			 (inline ,name))
		(defun ,name (a &rest more)
		  ,docstring
		  (cond ((null more) t)
			((,two-arg a (first more))
			 (apply (function ,name) more))
			(t nil)))
		(define-compiler-macro ,name (&whole form a &rest more)
		  (cond ((null more) t)
			((not (cdr more))
			 `(let ((#1=#:a ,a) (#2=#:b ,(first more)))
			    (,',two-arg #1# #2#)))
			((not (cddr more))
			 `(let ((#1# ,a) (#2# ,(first more))
				(#3=#:c ,(second more)))
			    (and (,',two-arg #1# #2#) (,',two-arg #2# #3#))))
			((not (cdddr more))
			 `(let ((#1# ,a) (#2# ,(first more))
				(#3# ,(second more)) (#4=#:d ,(third more)))
			    (and (,',two-arg #1# #2#) (,',two-arg #2# #3#)
				 (,',two-arg #3# #4#))))
			(t form))))))
  (frob cmp< two-arg-cmp< "Returns T if its arguments are in strictly increasing order according to CMP; NIL otherwise.")
  (frob cmp> two-arg-cmp> "Returns T if its arguments are in strictly decreasing order according to CMP; NIL otherwise.")
  (frob cmp= two-arg-cmp= "Returns T if all of its arguments are equal according to CMP; NIL otherwise.")
  (frob cmp<= two-arg-cmp<= "Returns T if its arguments are in strictly non-decreasing order according to CMP; NIL otherwise.")
  (frob cmp>= two-arg-cmp>= "Returns T if its arguments are in strictly non-increasing order according to CMP; NIL otherwise."))

(declaim (ftype (function (t &rest t) boolean) cmp/=)
	 (inline cmp/=))
(defun cmp/= (a &rest more)
  "Returns T if no two of its arguments are equal according to CMP; NIL otherwise."
  (if more
      (and (every (lambda (x) (two-arg-cmp/= a x)) more)
	   (apply #'cmp/= more))
      t))

(define-compiler-macro cmp/= (&whole form a &rest more)
  (cond ((null more) t)
	((not (cdr more))
	 `(let ((#1=#:a ,a) (#2=#:b ,(first more)))
	    (two-arg-cmp/= #1# #2#)))
	((not (cddr more))
	 `(let ((#1# ,a) (#2# ,(first more)) (#3=#:c ,(second more)))
	    (and (two-arg-cmp/= #1# #2#) (two-arg-cmp/= #1# #3#)
		 (two-arg-cmp/= #2# #3#))))
	(t form)))

#+CMU (ext:define-hash-table-test 'cmp= #'cmp= #'hash)
#+SBCL (sb-ext:define-hash-table-test cmp= hash)
#+CLISP (ext:define-hash-table-test cmp= cmp= hash)
