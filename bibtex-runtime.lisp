;; A BibTeX re-implementation in Common Lisp - runtime package
;; Copyright 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of version 2.1 of the GNU Lesser 
;; General Public License as published by the Free Software 
;; Foundation or any later version, as clarified by the preamble 
;; found in COPYING-preamble.txt. This preamble is in the style
;; of the Franz Inc. preamble at http://opensource.franz.com/preamble.html
;; with names and copyright holders altered accordingly.

(in-package bibtex-runtime)

;;; Error history

(defconstant +spotless-history+ 0)
(defconstant +warning-message+ 1)
(defconstant +error-message+ 2)
(defconstant +fatal-message+ 3)

(defvar *history* +spotless-history+)
(defvar *err-count* 0)

(defun mark-history (level)
  (cond ((> level *history*)
	 (setq *history* level
	       *err-count* 1))
	((= level *history*)
	 (incf *err-count*))))

(defun mark-warning ()
  (mark-history +warning-message+))

(defun mark-error ()
  (mark-history +error-message+))

(defun mark-fatal ()
  (mark-history +fatal-message+))

(defun bib-warn (format-control &rest args)
  "Emit a warning."
  (format *error-output* "Warning--")
  (apply #'format *error-output* format-control args)
  (terpri *error-output*)
  (mark-warning))

(defun bib-warn* (&rest strings)
  "Emit a warning consisting of the concatenation of STRINGS."
  (bib-warn "~{~A~}" strings))

(defun bib-error (format-control &rest args)
  "When there's a serious error parsing a BIB file, we flush
everything up to the beginning of the next entry."
  (apply #'format *error-output* format-control args)
  (format *error-output* "~&I'm skipping whatever remains of this command or entry~%")
  (mark-error)
  (throw 'bib-error nil))

(defun bib-fatal (format-control &rest args)
  (apply #'format *error-output* format-control args)
  (terpri *error-output*)
  (mark-fatal)
  (error "Fatal BibTeX error"))

;;; Interface to BibTeX entries

;;; Reading the database files

(defvar *bib-stream* nil)
(defvar *bib-macros* nil "A hashtable associating macro names with their definitions")
(defvar *bib-database* nil "A hashtable associating BibTeX keys with entries")
(defvar *bib-entries* nil "A list containing all requested BibTeX entries")
(defvar *bib-preamble* "" "A string accumulating all BibTeX @PREAMBLEs")
(defvar *bib-entry* nil)
(defvar *bib-entry-type-functions* nil
  "An alist mapping BibTeX entry types to formatter functions")

(defparameter *identifier-growth* 32)

(defun read-bib-identifier ()
  "Read an identifier from *BIB-STREAM*, returning it as a string, or
nil if no identifier could be read."
  (if (digit-char-p (peek-char t *bib-stream*))
      nil
      (let ((s (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
	(loop as char = (peek-char nil *bib-stream*)
	      until (or (whitespace-p char)
			(member char '(#\" #\# #\% #\' #\( #\) #\, #\= #\{ #\} )))
	      do (vector-push-extend (read-char *bib-stream*) s *identifier-growth*))
	(if (zerop (length s))
	    nil
	    s))))

(defun read-bib-database (stream)
  "Read a BibTeX database from STREAM, storing the entries in the
hash-table *BIB-DATABASE* and using/updating the macro hash-table
*BIB-MACROS*."
  (let ((*bib-stream* stream))
    (loop
     ;; skip everything up to at-sign
     (loop (let ((char (read-char stream nil nil)))
	     (cond ((not char) (return-from read-bib-database))
		   ((char= char #\@) (return)))))
     (catch 'bib-error 
       (let ((ident (read-bib-identifier)))
	 (cond
	   ((string-equal ident "COMMENT")
	    nil)		; do nothing
	   ((string-equal ident "PREAMBLE")
	    (process-bib-preamble-command))
	   ((string-equal ident "STRING")
	    (process-bib-string-command))
	   (t (process-bib-entry-command ident))))))))
  
(defun scan-balanced-braces (stream right-delimiter)
  "Scan STREAM for the RIGHT-DELIMITER character, skipping balanced
pairs of braces. Return a string of everything read, except for the
right delimiter."
  (let ((brace-level 0)
	(result (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop (let ((char (read-char stream nil nil)))
	    (cond
	      ((and (char= char right-delimiter)
		    (zerop brace-level))
	       (return result))
	      ((char= char #\{)
	       (incf brace-level))
	      ((char= char #\})
	       (if (zerop brace-level)
		   (bib-warn "Unbalanced braces")
		   (decf brace-level))))
	    (vector-push-extend char result 32)))))

(defun read-bib-field-token ()
  (case (peek-char t *bib-stream*)
    ((#\{)
     (read-char *bib-stream*)
     (scan-balanced-braces *bib-stream* #\}))
    ((#\")				; "
     (read-char *bib-stream*)
     (scan-balanced-braces *bib-stream* #\")) ; "
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (loop as char = (peek-char nil *bib-stream*)
	   while (digit-char-p char)
	   collecting (read-char *bib-stream*) into list
	   finally (return (coerce list 'string))))
    (otherwise
     (let ((macro (read-bib-identifier)))
       (unless macro
	 (bib-error "Expected a string, a number, or a macro name"))
       (let ((definition (gethash macro *bib-macros*)))
	 (cond
	   ((null definition)
	    (bib-warn "string name \"~A\" is undefined" macro)
	    "")
	   (t
	    definition)))))))

(defvar +bib-whitespace-character-list+
  '(#\Newline #\Space #\Linefeed #\Return #\Page #\Tab))

(defvar +bib-sep-character-list+
  '(#\~ #\-))

(defun whitespace-p (char)
  (member char +bib-whitespace-character-list+))

(defun sepchar-p (char)
  (member char +bib-sep-character-list+))

(defun compress-whitespace (s)
  "Compress non-null whitespace to a single space."
  (let ((whitespace-seen-p nil)
	(result (make-array (length s)
			    :element-type 'character
			    :fill-pointer 0)))
    (loop for char across (the string s)
	  do (cond ((not (whitespace-p char))
		    (vector-push char result)
		    (setq whitespace-seen-p nil))
		   ((not whitespace-seen-p)
		    (vector-push #\Space result)
		    (setq whitespace-seen-p t))))
    result))

(defun read-bib-field-value (field-p)
  "Read a list of field tokens from *BIB-STREAM* that define the field
value string and return the concatenation of all field tokens,
compressing non-null whitespace to a single space.  If FIELD-P is
non-nil, remove any leading or trailing whitespace."
  (let ((result (read-bib-field-token)))
    (loop (if (char= (peek-char t *bib-stream*)
		     #\#)
	      (progn
		(read-char *bib-stream*)
		(setq result (concatenate 'string result (read-bib-field-token))))
	      (return)))
    (if field-p
	(string-trim +bib-whitespace-character-list+ (compress-whitespace result))
	(compress-whitespace result))))

(defun read-bib-field (field-p)
  "Return two values, the name and the value."
  (let ((name (read-bib-identifier)))
    (unless name
      (bib-error "Expected a field name"))
    (unless (char= (peek-char t *bib-stream*)
		   #\=)
      (bib-error "Expected `=' sign"))
    (read-char *bib-stream*)
    (values name (read-bib-field-value field-p))))

(defun process-bib-string-command ()
  (let* ((open-char (peek-char t *bib-stream*))
	 (close-char
	  (case open-char
	    (#\{ #\})
	    (#\( #\))
	    (otherwise "Expected a `(' or `{' sign"))))
    (read-char *bib-stream*)
    (multiple-value-bind (name value)
	(read-bib-field nil)
      (setf (gethash name *bib-macros*)
	    value))
    (unless (char= (peek-char t *bib-stream*)
		    close-char)
      (bib-error "Expected `~A' sign" close-char))))

(defun process-bib-preamble-command ()
  (let* ((open-char (peek-char t *bib-stream*))
	 (close-char
	  (case open-char
	    (#\{ #\})
	    (#\( #\))
	    (otherwise "Expected a `(' or `{' sign"))))
    (read-char *bib-stream*)
    (setf *bib-preamble*
	  (concatenate 'string *bib-preamble* 
		       (read-bib-field-value nil)))
    (unless (char= (peek-char t *bib-stream*)
		   close-char)
      (bib-error "Expected `~A' sign" close-char))))


(defstruct bib-entry
  (type nil)
  (cite-key "")
  (sort-key% nil)
  (dict (make-hash-table :size 16 :test 'equalp) :read-only t))

(defun bib-entry-ref (key entry &optional default)
  (gethash key (bib-entry-dict entry) default))

(defun (setf bib-entry-ref) (value key entry &optional default)
  (declare (ignore default))
  (when (string= key "SORT.KEY$") (setf (bib-entry-sort-key% entry) nil))
  (setf (gethash key (bib-entry-dict entry)) value))

(defvar *generate-sort-key* #'identity)

(defun bib-entry-sort-key (entry)
  (or (bib-entry-sort-key% entry)
      (setf (bib-entry-sort-key% entry)
	  (funcall *generate-sort-key* (bib-entry-ref "SORT.KEY$" entry "")))))

(defmethod cmp ((a bib-entry) (b bib-entry))
  (cmp (bib-entry-sort-key a) (bib-entry-sort-key b)))

(defmethod hash ((entry bib-entry))
  (hash (bib-entry-sort-key entry)))

(defun process-bib-entry-command (entry-type)
  (let* ((open-char (peek-char t *bib-stream*))
	 (close-char
	  (case open-char
	    (#\{ #\})
	    (#\( #\))
	    (otherwise "Expected a `(' or `{' sign")))
	 key)
    (read-char *bib-stream*)
    (peek-char t *bib-stream*)
    (loop as char = (read-char *bib-stream* nil #\Newline)
	  until (or (char= char #\,)
		    (whitespace-p char))
	  collecting char into list
	  finally (if (char= char #\,) (unread-char char *bib-stream*))
	  (setq key (coerce list 'string)))
    (when (gethash key *bib-database*) 
      (bib-error "Repeated entry---~A" key))
    (unless (char= (peek-char t *bib-stream*) #\,)
      (bib-error "Expected `,' character"))
    (read-char *bib-stream*)
    (let ((entry (make-bib-entry)))
      (setf (bib-entry-type entry) (string-downcase entry-type))
      (setf (bib-entry-cite-key entry) key) 
      (loop (multiple-value-bind (name value)
		(read-bib-field t)
	      (setf (bib-entry-ref name entry)
		    value))
	    (let ((char (peek-char t *bib-stream*)))
	    (cond
	      ((char= char #\,)
	       (read-char *bib-stream*)
	       (when (char= (peek-char t *bib-stream*) close-char)
		 (read-char *bib-stream*)
		 (return)))
	      ((char= char close-char)
	       (read-char *bib-stream*)
	       (return))
	      (t (bib-error "Expected `,' or `~A'" close-char)))))
      (setf (gethash key *bib-database*) entry))))

;;; Writing BibTeX databases

(defun write-bib-entry (entry &optional (stream *standard-output*))
  (format stream "~&@~A{~A"
	  (bib-entry-type entry)
	  (bib-entry-cite-key entry))
  (loop for field being each hash-key in (bib-entry-dict entry)
	and value being each hash-value in (bib-entry-dict entry)
	do (format stream ",~%  ~A = {~A}" field value))
  (format stream "~%}~%"))

#||
(write-bib-entry (gethash "Corput" *bib-database*))
(loop for entry being each hash-value in *bib-database* do
      (write-bib-entry entry)) ||#

;;; Computing the cited entries

(defun merge-bib-entries (a b)
  "Return a fresh bib entry that merges A and B."
  (let ((entry (make-bib-entry)))
    (setf (bib-entry-type entry) (bib-entry-type a)
	  (bib-entry-cite-key entry) (bib-entry-cite-key a))
    (loop for key being each hash-key in (bib-entry-dict a)
	  and value being each hash-value in (bib-entry-dict a)
	  do (setf (bib-entry-ref key entry) value))
    (loop for key being each hash-key in (bib-entry-dict b)
	  and value being each hash-value in (bib-entry-dict b)
	  do (unless (bib-entry-ref key entry)
	       (setf (bib-entry-ref key entry) value)))
    entry))

(defun get-merged-bib-entry (key)
  "Compute a bib entry where all crossrefs have been merged in."
  (labels ((get-entry (key parent-keys)
	     (let ((entry (gethash key *bib-database*)))
	       (cond
		 ((not entry)
		  (bib-warn "I didn't find a database entry for ~A"
			    key)
		  nil)
		 (t
		  (let ((crossref (bib-entry-ref "CROSSREF" entry)))
		    (cond
		      ((not crossref)
		       entry)
		      ((member crossref parent-keys :test 'string-equal)
		       ;; circular cross reference
		       (bib-warn "I detected a circular cross-reference to ~A"
				 crossref)
		       nil)
		      (t
		       (let ((crossref-entry
			      (get-entry crossref (cons key  parent-keys))))
			 (if crossref-entry
			     (merge-bib-entries entry crossref-entry)
			     entry))))))))))
    (get-entry key '())))

(defun cited-bib-entries (cite-keys &key
			  (min-crossrefs 2))
  "Return a vector of the entries in *BIB-DATABASE* whose keys are
given in the list CITE-KEYS (if CITE-KEYS is the symbol T, return a
vector of all database entries.  When a crossref'd entry is referenced
at least :MIN-CROSSREFS times, it is included as a separate entry as
well."
  (let ((bib-entries (make-array 0 :adjustable t :fill-pointer 0)))
    (cond
      ((eql cite-keys t)
       (loop for key being each hash-key in *bib-database*
	     do (vector-push-extend (get-merged-bib-entry key) bib-entries 256)))
      (t
       (let ((crossref-hash (make-hash-table :test 'equalp))
	     (processed-keys '()))
	 ;; count how many times entries are cross-referenced
	 (labels ((count-crossrefs (key)
		    (unless (member key processed-keys :test 'string-equal)
		      (push key processed-keys)
		      (let ((entry (gethash key *bib-database*)))
			(when entry	; we will issue a warning
					; for non-existing keys later 
			  (let ((crossref (bib-entry-ref "CROSSREF" entry)))
			    (when (and crossref
				       (not (member crossref cite-keys
						    :test 'string-equal)))
			      (setf (gethash crossref crossref-hash)
				    (+ 1 (gethash crossref crossref-hash 0)))
			      (count-crossrefs crossref))))))))
	   (dolist (key cite-keys)
	     (count-crossrefs key)))
	   (dolist (key cite-keys)
	     (let ((entry (get-merged-bib-entry key)))
	       (when entry
		 (vector-push-extend entry bib-entries 256))))
	   (loop for key being each hash-key in crossref-hash
		 and count being each hash-value in crossref-hash
		 when (>= count min-crossrefs)
		 do (let ((entry (get-merged-bib-entry key)))
		      (when entry
			(vector-push-extend entry bib-entries 256)))))))
    (coerce bib-entries 'list)))

;;; BibTeX names

(defstruct bibtex-name 
  "BibTeX name, broken down into its components. Each component is a
list of name tokens (SEP-CHAR TOKEN-STRING)."
  first von last jr)

(defvar *bibtex-split-initials* t
  "If non-nil, BibTeX understands that there are two abbreviated first
names in names like `Padberg, M.W.'.  The original BibTeX 0.99c,
written by Oren Patashnik in the WEB language, thinks that `M.W.' is a
single first name; hence, in abbreviated format, the name becomes
`M. Padberg' instead of `M. W. Padberg'.")

(defvar *bibtex-split-initials-already-warned-hashtable* nil)

(defun tokenize-bibtex-name (name-string &key (start 0) (end nil))
  "Break a BibTeX name into name tokens."
  (unless end
    (setq end (length name-string)))
  ;; Remove leading whitespace and sepchars
  (do ()
      ((>= start end))
    (let ((char (elt name-string start)))
      (if (or (whitespace-p char)
	      (sepchar-p char))
	  (incf start)
	  (return))))
  ;; Remove trailing whitespace, sepchars and commas
  (do ()
      ((>= start end))
    (let ((char (elt name-string (- end 1))))
      (cond
	((or (whitespace-p char)
	     (sepchar-p char))
	 (decf end))
	((char= char #\,)
	 (bib-warn "Name has a comma at the end")
	 (decf end))
	(t (return)))))
  ;; Tokenize
  (do ((brace-level 0)
       (sep-char nil)
       (token-start nil)
       (tokens '())
       (index start (+ index 1)))
      ((>= index end)
       (if token-start
	   (setq tokens (cons (cons (or sep-char #\Space)
				    (subseq name-string token-start index))
			      tokens)))
       (nreverse tokens))
    (flet ((current-token () (cons (or sep-char #\Space)
				   (subseq name-string token-start index))))
      (let ((char (elt name-string index)))
	(cond
	  ((and (zerop brace-level)
		(char= char #\,))
	   (if token-start
	       (setq tokens (cons (current-token) tokens)
		     token-start nil))
	   (setq sep-char #\,))
	  ((and (zerop brace-level)
		(or (whitespace-p char)
		    (sepchar-p char)))
	   (if token-start
	       (setq tokens (cons (current-token) tokens)
		     token-start nil
		     sep-char nil))
	   (if (not sep-char)
	       (setq sep-char (if (whitespace-p char) #\Space char))))
	  ((and (char= char #\.)
		*bibtex-split-initials*
		(zerop brace-level)
		token-start
		(< (1+ index) end)
		(let ((next-char (char name-string (1+ index))))
		  (and (not (whitespace-p next-char)) 
		       (not (sepchar-p next-char))
		       (not (char= next-char #\,)))))
	   (let ((name (subseq name-string start (or end (length name-string)))))
	     (unless (gethash name *bibtex-split-initials-already-warned-hashtable*)
	       (bib-warn "Splitting the initials in the name `~A';~%  I suggest you add spaces between initials in the database entry" name)
	       (setf (gethash name  *bibtex-split-initials-already-warned-hashtable*) t)))
	   (setq tokens (cons (cons (or sep-char #\Space)
				    (subseq name-string token-start (+ index 1)))
			      tokens)
		 token-start nil
		 sep-char nil))	   	; nil sep-char is a weak #\Space
	  ((char= char #\{)
	   (incf brace-level)
	   (if (not token-start)
	       (setq token-start index)))
	  ((char= char #\})
	   (if (zerop brace-level)
	       (bib-error "Unbalanced braces")
	       (decf brace-level))
	   (if (not token-start)
	       (setq token-start index)))
	  (t
	   (if (not token-start)
	       (setq token-start index))))))))

(defun von-token-p (token)
  ;; From the BibTeX source code: "the von name
  ;; starts with the first nonlast token whose first brace-level-0 letter
  ;; is in lower case (for the purposes of this determination, an accented
  ;; or foreign character at brace-level-1 that's in lower case will do, as
  ;; well)."
  (let ((string (cdr token)))
    (and (> (length string) 0)
	 (with-input-from-string (s string)
	   (let ((brace-level 0))
	     (loop 
		for char = (read-char s nil nil)
		while char
		do (cond ((char= char #\{) 
			   (incf brace-level)
			   (when (and (= brace-level 1)
				      (char= (peek-char nil s nil #\Space) #\\))
			     ;; @<Check the special character (and |return|)@>=
			     (read-char s) ;consume backslash
			     ;; Check whether it is a special character; otherwise, 
			     ;; just ignore the control sequence.
			     (let* ((control-sequence (read-tex-control-sequence s))
				    (foreign-character (find-foreign-character control-sequence)))
			       (when foreign-character
				 (return (lower-case-p (char control-sequence 0)))))
			     ;; It is not a special character, so
			     ;; treat it as an accented character.  We
			     ;; now only scan till the end of the
			     ;; brace group; the first alphabetic
			     ;; character decides.  If nothing is
			     ;; found within the brace group, decide
			     ;; that is not a "von" token.
			     (loop 
				for char = (read-char s nil nil)
				while char 
				while (plusp brace-level)
				do (cond ((upper-case-p char) 
					  (return-from von-token-p nil))
					 ((lower-case-p char) 
					  (return-from von-token-p t))
					 ((char= char #\{)
					  (incf brace-level))
					 ((char= char #\})
					  (decf brace-level)))
				finally (return-from von-token-p nil))))
			 ((plusp brace-level)
			  (when (char= char #\}) 
			    (decf brace-level)))
			 ;; The first alphabetic char at brace-level 0 decides:
			 ((upper-case-p char) 
			  (return nil))
			 ((lower-case-p char) 
			  (return t)))))))))

(defun parse-bibtex-name (name-string &key (start 0) (end nil))
  "Break a BibTeX name into its components, returning a BIBTEX-NAME
structure."
  (unless end
    (setq end (length name-string)))
  (let* ((tokens (coerce (tokenize-bibtex-name name-string :start start :end end)
			 'vector))
	 (comma-1 (position #\, tokens :key #'car))
	 (comma-2 (and comma-1
		       (position #\, tokens :key #'car :start (+ comma-1 1))))
	 (too-many (and comma-2 (find #\, tokens :key #'car :start (+ comma-2 1)))))
    (flet ((token-list (start &optional end)
	     (unless end (setq end (length tokens)))
	     (coerce (subseq tokens start end) 'list)))
      (when too-many (bib-warn "Too many commas in name"))
      (when comma-1 (setf (car (elt tokens comma-1)) #\Space))
      (when comma-2 (setf (car (elt tokens comma-2)) #\Space))
      (cond
	(comma-1
	 ;; format is `von Last, [Jr.,] First', so find out where
	 ;; `von' and `Last' meet
	 (let* ((von-index
		 (and (> comma-1 1)
		      (position-if #'von-token-p tokens :from-end t
				   :end (- comma-1 1))))
		(von-end (if von-index (+ 1 von-index) 0)))
	   (make-bibtex-name :first (token-list (or comma-2 comma-1))
			     :von (token-list 0 von-end)
			     :last (token-list von-end comma-1)
			     :jr (token-list comma-1 (or comma-2 comma-1)))))
	(t
	 ;; format is `First von Last', so find out where these
	 ;; components meet
	 (let ((von-index (position-if #'von-token-p tokens)))
	   (if von-index
	       ;; we have a `von' component, so `First' is everything
	       ;; before the first `von', and `Last' is everything
	       ;; after the last `von'.
	       (let ((right-von-index (position-if #'von-token-p tokens :from-end t)))
		 (make-bibtex-name :first (token-list 0 von-index)
				   :von (token-list von-index (+ right-von-index 1))
				   :last (token-list (+ right-von-index 1))))
	       ;; we have no `von' component, so `Last' consists of
	       ;; all connected tokens at the end.
	       (let ((last-start (position-if #'(lambda (token)
						  (or (char= (car token) #\~)
						      (not (sepchar-p (car token)))))
					      tokens :from-end t)))
		 (make-bibtex-name :first (token-list 0 (or last-start 0))
				   :last (token-list (or last-start 0)))))))))))
  

(defun find-and-at-brace-level-0 (string &key (start 0) (end nil))
  "Return the index of the first `and' surrounded by non-null
whitespace at brace level 0 in STRING, bounded by :START and :END.  If
none found, return nil."
  (unless end
    (setq end (length string)))
  (do ((brace-level 0)
       (preceding-white nil (whitespace-p (elt string index)))
       (index start (+ 1 index)))
      ((>= index end) nil)
    (case (elt string index)
      (#\a (if (and (< (+ index 3) end)
		    (zerop brace-level)
		    preceding-white
		    (string-equal "and" string :start2 index :end2 (+ index 3))
		    (whitespace-p (elt string (+ index 3))))
	       (return-from find-and-at-brace-level-0 index)))
      (#\{ (incf brace-level))
      (#\} (if (zerop brace-level)
	       (bib-warn "Unbalanced braces")
	       (decf brace-level))))))

(defun parse-bibtex-name-list (names-string)
  "Parse a string containing BibTeX names, separated by the word `and'
surrounded by non-null whitespace, and return a list of BIBTEX-NAME
structures."
  (if (every #'whitespace-p names-string)
      '()
      (do ((start 0)
	   (name-list '()))
	  (nil)
	(let ((index (find-and-at-brace-level-0 names-string :start start)))
	  (setq name-list (cons (parse-bibtex-name names-string :start start :end index) name-list))
	  (if index
	      (setq start (+ index 4))
	      (return (nreverse name-list)))))))

(defvar *bibtex-long-token-length* 3
  "A BibTeX name token is considered `long' when it has at least this
many text characters.")  

(defvar *bibtex-long-name-length* 3
  "A BibTeX name component is considered `long' when it has at least this
many text characters.") 

(defun enough-text-chars (string min-length)
  ;; FIXME: Handle special characters (418)
  (>= (length string) min-length))

(defun format-bibtex-name-component (stream stream-string
				     tokens full inter-token-string)
  ;; From the BibTeX documentation:
  ;; A tie is the default space character between the last two tokens of
  ;; the name part, and between the first two tokens if the first token is
  ;; short enough; otherwise, a space is the default.
  ;;
  ;; CL-BibTeX remark: We note that BibTeX treats initial literal
  ;; characters in a level-1 brace group as belonging to the first
  ;; name token.  That is:
  ;;
  ;;  "C. A. J. Foooo" #1 "{ff}" format.name$ write$ newline$
  ;;    ==> C.~A.~J.
  ;;  "C. A. J. Foooo" #1 "{ll}, {ff}" format.name$ write$ newline$
  ;;    ==> Foooo, C.~A.~J.
  ;;  "C. A. J. Foooo" #1 "{ll}{, ff}" format.name$ write$ newline$
  ;;    ==> Foooo, C. A.~J.
  ;;
  ;; In the last example, ", C." is the first token, so that it is long enough, so it is not connected with a tie!
  ;;
  ;; This might be a bug in BibTeX, but we mimic this behaviour in
  ;; CL-BibTeX as well.
  ;;
  (do ((tokens tokens (cdr tokens))
       (token-index 0 (+ token-index 1)))
      ((null tokens))
    (let ((token (car tokens))
	  (*print-pretty* nil))
      (unless (zerop token-index)
	(cond
	  (inter-token-string
	   (princ inter-token-string stream))
	  (t
	   (unless full (princ #\. stream))
	   (cond
	     ((sepchar-p (car token))
	      (princ (car token) stream))
	     ((null (cdr tokens))	; last token
	      (princ #\~ stream))
	     ((not (enough-text-chars stream-string
				      *bibtex-long-token-length*))
	      (princ #\~ stream))
	     (t
	      (princ #\Space stream))))))
      (cond
	(full
	 (princ (cdr token) stream))
	(t ;; Abbreviate
	 (write-tex-group (list (first (parse-tex-string (cdr token))))
			  stream))))))

(defun format-bibtex-name (stream format-string bibtex-name)
  (let ((*print-pretty* nil)
	(string-output nil)
	(end (length format-string)))
    (cond
      ((null stream) (setq stream (make-string-output-stream)
			   string-output t))
      ((eq stream t) (setq stream *standard-output*)))
    (do ((index 0))
	((>= index end))
      (let ((c (char format-string index)))
	(cond
	  ((char= c #\{)
	   (incf index)
	   (let ((string (make-array 0
				     :element-type 'character
				     :adjustable t
				     :fill-pointer 0))
		 (any-output t))
	     (with-output-to-string (s string)
	       (do ((brace-level 1))
		   ((or (>= index end)
			(= brace-level 0)))
		 (setq c (char format-string index))
		 (incf index)
		 (cond ((and (alpha-char-p c)
			     (= brace-level 1))
			(let* ((tokens
				(case (char-upcase c)
				  (#\F (bibtex-name-first bibtex-name))
				  (#\V (bibtex-name-von bibtex-name))
				  (#\L (bibtex-name-last bibtex-name))
				  (#\J (bibtex-name-jr bibtex-name))
				  (otherwise
				   (error "The format string ~S has an illegal brace-level-1 letter"
					  format-string))))
			       (double-letter
				(and (< index end)
				     (char-equal c (char format-string index))))
			       (inter-token-string nil))
			  (when double-letter (incf index))
			  (when (null tokens)
			    (setq any-output nil))
			  (when (and (< index end)
				     (char= #\{ (char format-string index)))
			    ;; Get inter-token string
			    (incf brace-level)
			    (do ((i (+ index 1) (+ i 1)))
				((or (= brace-level 1) (>= i end))
				 (setq inter-token-string (subseq format-string
								  (+ index 1) (- i 1))
				       index i))
			      (case (char format-string i)
				(#\{ (incf brace-level))
				(#\} (if (zerop brace-level)
					 (error "Unbalanced braces in BibTeX format string ~S" format-string)
					 (decf brace-level))))))
			  (format-bibtex-name-component s string tokens
							double-letter inter-token-string)))
		       ((char= c #\{)
			(incf brace-level)
			(princ c s))
		       ((char= c #\})
			(if (zerop brace-level)
			    (error "Unbalanced braces in BibTeX format string ~S" format-string)
			    (decf brace-level))
			(unless (zerop brace-level)
			  (princ c s)))
		       (t (princ c s)))))
	     (when any-output
	       (when (and (>= (length string) 1)
			  (char= (char string (- (length string) 1)) #\~))
		 ;; Handle a discretionary tie
		 (cond
		   ((and (>= (length string) 2)
			 (char= (char string (- (length string) 2)) #\~))
		    ;; double tie, so remove one
		    (decf (fill-pointer string)))
		   ((not (enough-text-chars string (+ *bibtex-long-name-length* 1)))
		    ;; too short, keep the tie
		    nil)
		   (t
		    (setf (char string (- (length string) 1)) #\Space))))
	       (princ string stream))))
	  ((char= c #\})
	   (error "Unbalanced braces in BibTeX format string ~A" format-string))
	  (t
	   (princ c stream)
	   (incf index)))))
    (when string-output
      (get-output-stream-string stream))))  

(defun format-nth-bibtex-name (stream format-string names-string index)
  "Parse NAMES-STRING as an `and'-separated list of BibTeX names, pick
the name of given 1-based INDEX and format it to STREAM according to
the BibTeX-style FORMAT-STRING."
  (unless (> index 0)
    (bib-warn "Bad index: ~A" index)
    (return-from format-nth-bibtex-name ""))
  (let ((bibtex-names (parse-bibtex-name-list names-string)))
    (when (> index (length bibtex-names))
      (if (zerop (length bibtex-names))
	  (bib-warn "There is no name in ~S" names-string)
	  (bib-warn "There aren't ~A names in ~S" index names-string))
      (return-from format-nth-bibtex-name ""))
    (format-bibtex-name stream format-string (elt bibtex-names (- index 1))))) 

(defun num-bibtex-names (names-string)
  (length (parse-bibtex-name-list names-string)))

;;; Reading the AUX files

(defvar *aux-file-commands*
  '(("citation" . aux-citation-command)
    ("bibdata" . aux-bibdata-command)
    ("bibstyle" . aux-bibstyle-command)
    ("@input" . aux-input-command)))

;; Variables used during READ-AUX-FILE
(defvar *citation-seen-p* nil "Non-nil if a \citation command has been seen in an AUX file.")
(defvar *bibdata-seen-p* nil "Non-nil if a \bibdata command has been seen in an AUX file.")
(defvar *aux-stream* nil "The stream corresponding to the current AUX file.")

;; The results of READ-AUX-FILE
(defvar *bib-style* nil "The requested BibTeX style.")
(defvar *bib-files* '()
  "List of BibTeX database files to be read.")
(defvar *cite-all-entries* nil
  "Non-nil if all BibTeX entries are cited.")
(defvar *cite-keys* '()
  "List of cited BibTeX keys.")

(defun aux-error (format-control &rest args)
  (apply #'format *error-output* format-control args)
  
  (mark-error)
  (throw 'aux-error nil))

(defun tex-alpha-char-p (char &key (at-is-letter nil))
  (or (alpha-char-p char)
      (and at-is-letter (char= char #\@))))

(defun read-tex-control-sequence (stream &key (skip-whitespace t)
				  (at-is-letter nil))
  "Read a TeX control sequence from STREAM, assuming that the escape
character (\\) has already been read.  In the case of a control word,
trailing whitespace is flushed if :SKIP-WHITESPACE is non-nil."
  (let ((char (read-char stream nil #\Space)))
    (if (tex-alpha-char-p char :at-is-letter at-is-letter)
	(let ((result (make-array 1 :element-type 'character
				  :fill-pointer t :adjustable t
				  :initial-element char)))
	  (loop for char = (peek-char nil stream nil #\Space)
		while (tex-alpha-char-p char :at-is-letter at-is-letter)
		do (vector-push-extend (read-char stream) result 32))
	  (when skip-whitespace
	    (peek-char t stream nil nil))
	  result)
	(string char))))

(defun get-aux-command-and-process ()
  "Read a TeX control sequence from *AUX-STREAM*.  If the sequence is
found in *AUX-FILE-COMMANDS*, call the associated function."
  (let* ((ctl (read-tex-control-sequence *aux-stream*
					 :skip-whitespace nil
					 :at-is-letter t))
	 (command (assoc ctl *aux-file-commands* :test 'string=)))
    (when command
      (if (catch 'aux-error
	    (funcall (cdr command))
	    t)
	  (let ((line-end (read-line *aux-stream* nil "")))
	    (unless (every #'whitespace-p line-end)
	      (format *error-output* "~&Trailing garbage after AUX-file command: `~A'~%"
		      line-end)))
	  (progn
	    (format *error-output* "~&I'm skipping whatever remains of this command~%")
	    (read-line *aux-stream* nil ""))))))

(defvar *aux-file-level* 0)
	   
(defun read-aux-file-recursively (name)
  (let ((full-name (kpathsea:find-file name)))
    (unless full-name
      (bib-fatal "I couldn't find auxiliary file: ~S" name))
    (with-open-file (*aux-stream* full-name :if-does-not-exist nil)
      (unless *aux-stream*
	(bib-fatal "I couldn't open auxiliary file: ~S" name))
      (if (zerop *aux-file-level*)
	  (format *error-output* "~&The top-level auxiliary file: ~A~%" full-name)
	  (format *error-output* "~&A level-~D auxiliary file: ~A~%" *aux-file-level* full-name))
      (loop as char = (peek-char nil *aux-stream* nil nil)
	    while char
	    do (cond
		 ((char= char #\\)
		  (read-char *aux-stream*)
		  (get-aux-command-and-process))
		 (t (read-line *aux-stream*)))))))

(defun read-aux-file (name)
  "Read an AUX file, modifying *CITE-KEYS*, *CITE-ALL-ENTRIES*,
*BIB-FILES*, and *BIB-STYLE*."
  (let ((*citation-seen-p* nil)
	(*bibdata-seen-p* nil)
	(*aux-file-level* 0))
    (read-aux-file-recursively name)
  ;; check everything ok
  (flet ((aux-end-error (what)
	   (format *error-output* "~&I found no ~A while reading the AUX file~%" what)
	   (mark-error)))
    (unless *citation-seen-p*
      (aux-end-error "\\citation commands"))
    (unless *bibdata-seen-p*
      (aux-end-error "\\bibdata command"))
    (unless *bib-style*
      (aux-end-error "\\bibstyle command")))
  (setq *bib-files* (nreverse *bib-files*)
	*cite-keys* (nreverse *cite-keys*))))
  
(defun aux-input-command ()
  "Process an AUX-file \\@input command."
  (unless (char= (read-char *aux-stream* nil #\Space) #\{)
    (aux-error "Expected `{'"))
  (let ((file-name (scan-balanced-braces *aux-stream* #\}))
	(*aux-file-level* (1+ *aux-file-level*)))
    (read-aux-file-recursively file-name)))

(defun aux-bibstyle-command ()
  "Process an AUX-file \\bibstyle command."
  (when *bib-style*
    (aux-error "Illegal, another \\bibstyle command"))
  (unless (char= (read-char *aux-stream* nil #\Space) #\{)
    (aux-error "Expected `{'"))
  (setq *bib-style* (scan-balanced-braces *aux-stream* #\})))

(defun scan-to-delimiter (stream delimiters)
  "Read characters from STREAM until a character in the list
DELIMITERS is found.  Return a string of these characters, excluding
the delimiter, which is left in the stream."
  (let ((s (make-array 0 :element-type 'character
		       :adjustable t :fill-pointer t)))
    (loop as char = (peek-char nil stream nil nil)
	  until (or (null char)
		    (member char delimiters))
	  do (vector-push-extend (read-char stream) s 64))
    s))

(defun aux-bibdata-command ()
  (when *bibdata-seen-p*
    (aux-error "Illegal, another \\bibdata command"))
  (setq *bibdata-seen-p* t)
  (unless (char= (read-char *aux-stream* nil #\Space) #\{)
    (aux-error "Expected `{'"))
  (loop (let ((data (scan-to-delimiter *aux-stream* '(#\, #\}))))
	  (push data *bib-files*)
	  (unless (char= (read-char *aux-stream* nil #\Space) #\,)
	    (return)))))

(defun aux-citation-command ()
  (setq *citation-seen-p* t)
  (unless (char= (read-char *aux-stream* nil #\Space) #\{)
    (aux-error "Expected `{'"))
  (loop (let ((key (scan-to-delimiter *aux-stream* '(#\, #\}))))
	  (cond
	    ((string= key "*")
	     (when *cite-all-entries*
	       (aux-error "Multiple inclusions of entire database"))
	     (setq *cite-all-entries* t))
	    (t
	     ;; fixme: detect duplicates
	     (unless (member key *cite-keys* :test 'string=)
	       (push key *cite-keys*))))
	  (unless (char= (read-char *aux-stream* nil #\Space) #\,)
	    (return)))))

;;; Writing the BBL file

(defvar *bbl-output* nil
  "The stream corresponding to the formatted bibliography (BBL) file.")

(defvar *bbl-min-print-line* 3
  "Minimum line length in the formatted bibliography (BBL) file.")

(defvar *bbl-max-print-line* 79
  "Maximum .bbl line length.  BBL-PRINT breaks lines so that no more
than this many characters apear on each output line.  If nil,
BBL-PRINT will not break lines.")

(defvar *bbl-line-buffer* nil)

(defun bbl-print (string)
  "Add STRING to the BBL output buffer.  If there are enough
characters present in the output buffer, it writes one or more lines
out to the *BBL-OUTPUT* stream.  It may break a line at any whitespace
character it likes, but if it does, it will add two spaces to the next
output line.  If there's no whitespace character to break the line at,
we break it just before *BBL-MAX-PRINT-LINE*, append a comment
character (%), and don't indent the next line."
  (loop for char across string do
       (vector-push-extend char *bbl-line-buffer* 128))
  (when *bbl-max-print-line*
    (let ((*print-pretty* nil))
      (loop while (> (length *bbl-line-buffer*) *bbl-max-print-line*) do
	 ;; Break that line
	   (let ((white-space-index
		  (position-if #'whitespace-p *bbl-line-buffer*
			       :start *bbl-min-print-line*
			       :end (+ *bbl-max-print-line* 1)
			       :from-end t)))
	     (cond
	       (white-space-index	; have a whitespace character
		(let ((non-space-index
		       (position-if (complement #'whitespace-p) *bbl-line-buffer*
				    :end white-space-index
				    :from-end t)))
		  (when non-space-index	; ignore a line of just whitespace
		    (princ (subseq *bbl-line-buffer* 0 (+ non-space-index 1))
			   *bbl-output*)
		    (terpri *bbl-output*)
		    ;; shift the rest back
		    (replace *bbl-line-buffer* *bbl-line-buffer*
			     :start1 2 :start2 (+ white-space-index 1))
		    ;; start the next line with two spaces
		    (replace *bbl-line-buffer* "  " :start1 0)
		    (decf (fill-pointer *bbl-line-buffer*)
			  (- (+ white-space-index 1) 2)))))
	       (t ; there's no whitespace character to break the line at
		(princ (subseq *bbl-line-buffer* 0 (- *bbl-max-print-line* 1))
		       *bbl-output*)
		(princ "%" *bbl-output*)
		(terpri *bbl-output*)
		(replace *bbl-line-buffer* *bbl-line-buffer*
			 :start1 0 :start2 (- *bbl-max-print-line* 1))
		(decf (fill-pointer *bbl-line-buffer*)
		      (- *bbl-max-print-line* 1)))))))))

(defun bbl-terpri ()
  ;; Trim trailing whitespace
  (loop while (and (> (length *bbl-line-buffer*) 0)
		   (whitespace-p (char *bbl-line-buffer*
					(- (length *bbl-line-buffer*) 1))))
	do (decf (fill-pointer *bbl-line-buffer*)))
  (let ((*print-pretty* nil))
    (princ *bbl-line-buffer* *bbl-output*)
    (terpri *bbl-output*))
  (setf (fill-pointer *bbl-line-buffer*) 0)
  nil)

(defun bbl-flush ()
  (unless (zerop (length *bbl-line-buffer*))
    (bbl-terpri)))

(defmacro with-bbl-output ((stream) &body body)
  `(let ((*bbl-output* ,stream)
	 (*bbl-line-buffer* (make-array 0
					:element-type 'character
					:adjustable t
					:fill-pointer 0)))
    (multiple-value-prog1 (progn ,@body)
      (bbl-flush))))

;;;

(defvar *min-crossrefs* 2 "When a crossref'd entry is referenced at
least *MIN-CROSSREFS* times, it is included as a separate entry as
well.")

(defun parse-equivalent-entries-string (string)
  (mapcar (lambda (s)
	    (string-trim +bib-whitespace-character-list+ s))
	  (split-sequence:split-sequence #\, string)))
;; (parse-equivalent-entries-string "newbar,barvinok-2006-ehrhart-quasipolynomial")
;; (parse-equivalent-entries-string "  newbar ,  barvinok-2006-ehrhart-quasipolynomial ")

(defun compute-bib-equivalence-classes ()
  "Return a list of lists of equivalent keys."
  (let ((marks (make-hash-table :test 'equalp))
	(components nil)
	(neighbors-hash (make-hash-table :test 'equalp)))
    ;; Compute the neighbors (symmetric)
    (loop for anchor being each hash-key in *bib-database* using (hash-value entry)
       do (let* ((keys-string (bib-entry-ref "EQUIVALENT-ENTRIES" entry))
		 (new-equivalent-keys (and keys-string
					   (parse-equivalent-entries-string keys-string))))
;; 	    (when new-equivalent-keys
;; 	      (format *error-output* "~&;; Equivalent to ~A:~{ ~A~}~%" anchor new-equivalent-keys))
	    (setf (gethash anchor neighbors-hash) 
		  (union new-equivalent-keys (gethash anchor neighbors-hash)))
	    (dolist (key new-equivalent-keys)
	      (pushnew anchor (gethash key neighbors-hash) :test 'equalp))))
    (loop for anchor being each hash-key in neighbors-hash using (hash-value neighbors)
       unless (gethash anchor marks) 
       do (let ((component (list anchor)))
	    (labels ((visit (node predecessors)
		       (setf (gethash node marks) t)
		       (dolist (neighbor-node neighbors)
			 (cond
			   ((gethash neighbor-node marks)) ; already marked
			   (t
			    (push neighbor-node component)
			    (visit neighbor-node (cons node predecessors)))))))
	      (visit anchor nil)
	      (push component components))))
    components))
;; (let ((*bib-database* (make-hash-table :test 'equalp)) (*bib-macros* (make-hash-table :test #'equalp)) (*bib-files* '("iba-bib" "weismant" "barvinok"))) (read-all-bib-files) (break) (compute-bib-equivalence-classes))

(defun read-all-bib-files ()
  (loop for file in *bib-files*
     for file-number from 1
     do (let ((expanded-file (kpathsea:find-file (concatenate 'string file ".bib"))))
	  (if (not expanded-file)
	      (format *error-output* "I couldn't find database file `~A.bib'" file)
	      (with-open-file (s expanded-file :if-does-not-exist nil)
		(if (not s)
		    (format *error-output* "I couldn't open database file `~A.bib'" expanded-file)
		    (progn
		      (format *error-output* "~&Database file #~D: ~A~%" file-number expanded-file)
		      (read-bib-database s))))))))

(defparameter *check-multiple-cited-equivalent-entries* t
  "If true, perform the following extension of BibTeX behavior:
The attribute EQUIVALENT-ENTRIES of a bibliographic entry
specifies the keys of other bibliographic entries that are to be
considered equivalent.  We compute the equivalence classes
spanned by these equivalences, and warn if multiple equivalent
keys are used in one document (we warn because both we and the
original BibTeX will produce a bibliography that contains several
copies of that entry).")

(defun check-multiple-cited-equivalent-entries (bib-entries)
  (let ((equivalence-classes
	 (compute-bib-equivalence-classes))
	(bib-keys (mapcar (lambda (x) (bib-entry-cite-key x)) bib-entries)))
    (loop for class in equivalence-classes
       do (let ((cited-equivalent-keys
		 (remove-if-not (lambda (key) (member key bib-keys :test 'equalp))
				class)))
	    (when (> (length cited-equivalent-keys) 1)
	      (bib-warn "These equivalent entries have been used:~{ ~A~}" 
			cited-equivalent-keys))))))

(defun read-all-bib-files-and-compute-bib-entries ()
  (read-all-bib-files)
  (let ((bib-entries
	 (cited-bib-entries (if *cite-all-entries* t *cite-keys*)
			    :min-crossrefs *min-crossrefs*)))
    (when *check-multiple-cited-equivalent-entries*
      (check-multiple-cited-equivalent-entries bib-entries))
    bib-entries))

;;; Misc functions

(defun add-period-unless-sentence-end (string)
  "Add a period to STRING unless it is empty, or its last
non-right-brace character is a period, question mark or exclamation
mark."
  (cond
    ((zerop (length string)) "")
    ((member (find #\} string :from-end t :test (complement #'char=))
	     '(#\. #\? #\!))
     string)
    (t (concatenate 'string string "."))))

(defun empty-field-p (string)
  (or (null string)
      (every #'whitespace-p string)))

(defun bibtex-substring (s start &optional (count most-positive-fixnum))
  "A substring function compatible with BibTeX's substring$."
  (cond
    ((or (> start (length s))
	 (< start (- (length s)))
	 (<= count 0))
     "")
    ((>= start 0)			; take count chars from start
     (subseq s (max 0 (- start 1)) (min (length s) (+ (- start 1) count))))
    (t
     (subseq s (max 0 (- (length s) (- start) (- count 1)))
	     (+ (length s) start 1)))))

(defun read-tex-group (stream)
  "Read TeX tokens from STREAM until a `}' character or end-of-file is
found.  Return a list of the tokens, where characters represent
ordinary characters, strings represent control sequences (without the
escape character) and nested lists represent TeX groups."
  (loop as char = (read-char stream nil nil)
	until (or (null char)
		  (char= char #\}))
	collecting (case char
		     (#\{ (read-tex-group stream))
		     (#\\ (read-tex-control-sequence stream))
		     (t   char))))

(defun parse-tex-string (string)
  "Parse the STRING containing TeX {groups} and \\controlsequences.
Return a list whose elements are characters, strings representing
control sequences or sub-lists representing groups."
  (with-input-from-string (s string)
    (read-tex-group s)))

(defun write-tex-group (group &optional (stream *standard-output*)
			no-terminate-p)
  "Write the TeX GROUP to STREAM.  If a control word occurs at the end
of the group, terminate it with whitespace unless NO-TERMINATE-P is
true."
  (let ((*print-pretty* nil))
    (loop for (token . tail) on group
       do (etypecase token
	    (character (princ token stream))
	    (list (princ "{" stream)
		  (write-tex-group token stream t)
		  (princ "}" stream))
	    (string (princ "\\" stream)
		    (princ token stream)
		    (unless (and (= (length token) 1)
				 (not (alpha-char-p (char token 0))))
		      ;; Control word, must be terminated
		      (unless no-terminate-p ; unless otherwise requested
			(when (or (null tail)
				  (and (characterp (car tail))
				       (alpha-char-p (car tail))))
					; when a character or nothing (unknown stuff) follows
			  (princ " " stream)))))))))

(defun for-all-tex-tokens (function string-or-group)
  "Call FUNCTION for every token in the given TeX string (a string or
a TeX-group).  The second argument passed to FUNCTION is the
bracelevel."
  (let ((group (etypecase string-or-group
		 (string (parse-tex-string string-or-group))
		 (list string-or-group))))
    (labels ((apply-function (group brace-level)
	       (dolist (token group)
		 (etypecase token
		   ((or character string)
		    (funcall function token brace-level))
		   (list
		    (apply-function token (+ 1 brace-level)))))))
      (apply-function group 0))))		 

(defmacro do-tex-tokens ((token string-or-group
				&optional (brace-level (gensym) brace-level-p))
			 &body body)
  "Perform BODY on every token in the given TeX string (a string or a
TeX-group)."
  `(for-all-tex-tokens (lambda (,token ,brace-level)
			 ,@(if brace-level-p
			       '()
			       `((declare (ignore ,brace-level))))
			 ,@body)
    ,string-or-group))

(defstruct foreign-character
  name		      ; a string representing a TeX control sequence
  uppercase	      ; list of tokens for the uppercase version of it
  lowercase	      ; likewise
  purification)	      ; a string that is the purified version of it

(defvar *foreign-characters* ()
  "A list of FOREIGN-CHARACTER structures.")

(defun define-foreign-character (&rest args)
  (let ((foreign-character (apply #'make-foreign-character args)))
    (push foreign-character *foreign-characters*)
    foreign-character))

(define-foreign-character :name "i"  :purification "i"  :uppercase '(#\I))
(define-foreign-character :name "j"  :purification "j"  :uppercase '(#\J))
(define-foreign-character :name "ae" :purification "ae" :uppercase '("AE"))
(define-foreign-character :name "AE" :purification "AE" :lowercase '("ae"))
(define-foreign-character :name "oe" :purification "oe" :uppercase '("OE"))
(define-foreign-character :name "OE" :purification "OE" :lowercase '("oe"))
(define-foreign-character :name "aa" :purification "a"  :uppercase '("AA"))
(define-foreign-character :name "AA" :purification "A"  :lowercase '("aa"))
(define-foreign-character :name "o"  :purification "o"  :uppercase '("O"))
(define-foreign-character :name "O"  :purification "O"  :lowercase '("o"))
(define-foreign-character :name "l"  :purification "l"  :uppercase '("L"))
(define-foreign-character :name "L"  :purification "L"  :lowercase '("l"))
(define-foreign-character :name "ss" :purification "ss" :uppercase '(#\S #\S))

(defun find-foreign-character (name)
  (let ((tail (member name *foreign-characters*
		      :key 'foreign-character-name
		      :test 'string=)))
    (if (null tail)
	nil
	(car tail))))

(defun bibtex-string-purify (string)
  "Remove nonalphanumeric characters except for whitespace and
sep-char characters (these get converted to a space) and removes
certain alphabetic characters contained in the control sequences
associated with a special character."
  ;; When the original BibTeX sees a "special character", i.e., a
  ;; control sequence immediately following a level-1 left brace,
  ;; purification is put into "special-character mode", where all
  ;; non-alphanumeric characters (including white-space and sep-chars)
  ;; are removed.
  ;;
  ;; This behavior of the original BibTeX also exposes a bug in
  ;; gerplain.sty, where the sort order is messed up.  To compute the
  ;; sort key, gerplain.sty formats author names in the form "{\sc
  ;; LAST  FIRST}" and passes this to PURIFY$.  This results in
  ;; "LASTFIRST".  Thus the order of "Geyer, Foo" and "Ge, Yigong"
  ;; comes out wrong.
  ;;
  ;; FIXME: CL-BibTeX currently does not handle this special case.
  ;; Therefore, gerplain.sty erroneously works correctly in CL-BibTeX.
  (with-output-to-string (s)
    (do-tex-tokens (token string)
      (etypecase token
	(character
	 (cond ((alphanumericp token)
		(princ token s))
	       ((or (sepchar-p token)
		    (whitespace-p token))
		(princ #\Space s))))
	(string
	 (let ((foreign-character (find-foreign-character token)))
	   (when foreign-character
	     (princ (foreign-character-purification foreign-character) s))))))))

(defun upcase-foreign-character (token)
  "TOKEN is a string, designating a TeX control sequence.  Return a
list of tokens that are the uppercase replacement."
  (let ((foreign-character (find-foreign-character token)))
    (if foreign-character
	(or (foreign-character-uppercase foreign-character)
	    (list token))
	(list token))))

(defun downcase-foreign-character (token)
  "TOKEN is a string, designating a TeX control sequence.  Return a
list of tokens that are the lowercase replacement."
  (let ((foreign-character (find-foreign-character token)))
    (if foreign-character
	(or (foreign-character-lowercase foreign-character)
	    (list token))
	(list token))))

(defun tex-group-change-case (group char-fun foreign-character-fun
			      foreign-character-char-fun after-group-thunk)
  (mapcan (lambda (token)
	    (etypecase token
	      (character
	       (list (funcall char-fun token)))
	      (list
	       (cond
		 ((and token
		       (stringp (first token)))
		  ;; Treat this brace group as a special character,
		  ;; i.e., upcase some control sequences and upcase
		  ;; ordinary characters, but don't recurse
		  (labels
		      ((change-case-recursively (group)
			 (list (mapcan (lambda (token)
					 (etypecase token
					   (character
					    (list (funcall foreign-character-char-fun token)))
					   (string
					    (funcall foreign-character-fun token))
					   (list
					    (change-case-recursively token))))
				       group))))
		    (prog1
			(change-case-recursively token)
		      (funcall after-group-thunk))))
		 (t
		  ;; An ordinary brace group, keep as is
		  (prog1
		      (list token)
		    (funcall after-group-thunk)))))
	      (string
	       (prog1 (funcall foreign-character-fun token)
		 (funcall after-group-thunk)))))
	  group))

(defun tex-group-upcase (group)
  (tex-group-change-case group
			 #'char-upcase #'upcase-foreign-character
			 #'char-upcase #'values))
			      
(defun bibtex-string-upcase (string)
  "Convert to upper case all letters in STRING at brace-level 0.
Also handle special characters in a very complicated way."
  (with-output-to-string (s)
    (write-tex-group (tex-group-upcase (parse-tex-string string))
		     s)))

(defun tex-group-downcase (group)
  (tex-group-change-case group
			 #'char-downcase #'downcase-foreign-character
			 #'char-downcase #'values))

(defun bibtex-string-downcase (string)
  "Convert to lower case all letters in STRING at brace-level 0.
Also handle special characters in a very complicated way."
  (with-output-to-string (s)
    (write-tex-group (tex-group-downcase (parse-tex-string string))
		     s)))

(defun tex-group-titledowncase (group)
  (let ((colon-state 'colon-and-whitespace))
    (tex-group-change-case group
			   (lambda (char)
			     (prog1 
				 (if (eq colon-state 'colon-and-whitespace)
				     char
				     (char-downcase char))
			       (cond ((char= char #\:)
				      (setq colon-state 'colon))
				     ((and (whitespace-p char)
					   (eq colon-state 'colon))
				      (setq colon-state 'colon-and-whitespace))
				     (t
				      (setq colon-state nil)))))
			   (lambda (control-sequence)
			     (if (eq colon-state 'colon-and-whitespace)
				 (list control-sequence)
				 (downcase-foreign-character control-sequence)))
			   (lambda (char)
			     (if (eq colon-state 'colon-and-whitespace)
				 char
				 (char-downcase char)))
			   (lambda ()
			     (setq colon-state nil)))))

(defun bibtex-string-titledowncase (string)
  "Convert to lower case all letters except the very first character
in the STRING, which it leaves alone, and except the first character
following any `:' and then non-null whitespace, which it also leaves
alone.  Only those letters at brace-level 0 are affected.  Also handle
special characters in a very complicated way."
  (with-output-to-string (s)
    (write-tex-group (tex-group-titledowncase (parse-tex-string string))
		     s)))

#+check-bibtex
(progn
  (assert (string= (bibtex-string-titledowncase "AB{\\'O}DE{\\AE}FG: DEF:GHI")
		   "Ab{\\'o}de{\\ae}fg: Def:ghi"))
  (assert (string= (bibtex-string-titledowncase "{\\\"U}ber")
		   "{\\\"U}ber"))
  (assert (string= (bibtex-string-upcase "Erd{\\H{o}}s")
		   "ERD{\\H{O}}S")))
	  
		   

(defun bibtex-string-prefix (string num-tokens)
  "The BibTeX TEXT.PREFIX$ function."
  ;; Takes the first NUM-TOKENS characters (in same way as
  ;; TEXT.LENGTH$ counts the length of a string) and cuts off the
  ;; string after them.  If unclosed braces remain, close them.
  (let ((group (parse-tex-string string)))
    (with-output-to-string (s)
      (write-tex-group 
       (subseq group 0 (min num-tokens (length group)))
       s))))

;;; The approximate calculation of text widths

(defvar +cmr10-character-widths+
  '((#\Space . 278) (#\! . 278) (#\" . 500) (#\# . 833) (#\$ . 500) (#\% . 833)
    (#\& . 778) (#\' . 278) (#\( . 389) (#\) . 389) (#\* . 500) (#\+ . 778)
    (#\, . 278) (#\- . 333) (#\. . 278) (#\/ . 500) (#\0 . 500) (#\1 . 500)
    (#\2 . 500) (#\3 . 500) (#\4 . 500) (#\5 . 500) (#\6 . 500) (#\7 . 500)
    (#\8 . 500) (#\9 . 500) (#\: . 278) (#\; . 278) (#\< . 278) (#\= . 778)
    (#\> . 472) (#\? . 472) (#\@ . 778) (#\A . 750) (#\B . 708) (#\C . 722)
    (#\D . 764) (#\E . 681) (#\F . 653) (#\G . 785) (#\H . 750) (#\I . 361)
    (#\J . 514) (#\K . 778) (#\L . 625) (#\M . 917) (#\N . 750) (#\O . 778)
    (#\P . 681) (#\Q . 778) (#\R . 736) (#\S . 556) (#\T . 722) (#\U . 750)
    (#\V . 750) (#\W . 1028) (#\X . 750) (#\Y . 750) (#\Z . 611) (#\[ . 278)
    (#\\ . 500) (#\] . 278) (#\^ . 500) (#\_ . 278) (#\` . 278) (#\a . 500)
    (#\b . 556) (#\c . 444) (#\d . 556) (#\e . 444) (#\f . 306) (#\g . 500)
    (#\h . 556) (#\i . 278) (#\j . 306) (#\k . 528) (#\l . 278) (#\m . 833)
    (#\n . 556) (#\o . 500) (#\p . 556) (#\q . 528) (#\r . 392) (#\s . 394)
    (#\t . 389) (#\u . 556) (#\v . 528) (#\w . 722) (#\x . 528) (#\y . 528)
    (#\z . 444) (#\{ . 500) (#\| . 1000) (#\} . 500) (#\~ . 500)
    ("ss" . 500) ("ae" . 722) ("oe" . 778) ("AE" . 903) ("OE" . 1014))
  "An alist associating characters with their widths.  The widths here
are taken from Stanford's June '87 cmr10 font and represent hundredths
of a point (rounded), but since they're used only for relative
comparisons, the units have no meaning.")

(defun bibtex-string-width (string &key (widths +cmr10-character-widths+))
  "The BibTeX WIDTH$ function.
Compute the approximate width of STRING by summing the WIDTHS of the
individual characters.  BibTeX special characters are handled
specially."
  ;; The original BibTeX handles special chars like this: WIDTH$ sums
  ;; up the widths of all characters, including braces.  Special
  ;; characters (backslash on bracelevel 1, followed by a control word
  ;; or a control char) are handled like this: The widths of \ss, \ae,
  ;; \oe, \AE, \OE are taken from the table; otherwise the width of
  ;; the first letter of the control sequence is taken.
  (let ((width 0))
    (do-tex-tokens (token string)
      (let ((assoc (assoc token widths :test 'equal)))
	(cond
	  (assoc
	   (incf width (cdr assoc)))
	  ((stringp token)
	   (unless (string= token "")
	     (let ((assoc (assoc (char token 0) widths)))
	       (when assoc
		 (incf width (cdr assoc)))))))))
    width))
  
#||

(setq *bib-macros* (make-hash-table))
(setq *bib-database* (make-hash-table :test #'equalp))

#+ignore (with-input-from-string (s "@string{jan=\"Januar\"}
@string(feb={Febr{u}ar})@article{the-key, xyzzyz=\"Foo\"}")
  (read-bib-database s))

(with-open-file (s "/home/mkoeppe/cvs/iba-papers/iba-bib.bib")
  (read-bib-database s))

(let ((*bibtex-split-initials* t))
  (parse-bibtex-name-list (bib-entry-ref 'editor (gethash "johnson-trick-96" *bib-database*))))

(defun show-author (id)
  (let ((*bibtex-split-initials* t))
    (parse-bibtex-name-list (bib-entry-ref "author" (gethash id *bib-database*)))))

(let ((*bibtex-split-initials* t))
  (remove-duplicates
   (sort 
    (loop for entry being each hash-value in *bib-database*
	  appending (mapcar #'(lambda (name) (format-bibtex-name nil "{f.~}{vv~}{ll}{, jj}" name))
			    (parse-bibtex-name-list (bib-entry-ref "author" entry))))
    #'string<=)
   :test #'equal))
   
  

(format-bibtex-name t "{ff} {ll}" (first (show-author "schnorr-euchner94")))

(parse-bibtex-name-list '())

(format-bibtex-name t "{f.} {l.}" (parse-bibtex-name "Matiyasevich, {\\relax{Yu}}ri V."))
(format-bibtex-name t "{f.} {l.}" (parse-bibtex-name "Matiyasevich, {\\'E}"))

(parse-bibtex-name "Jes{\'u}s A. {D}e Loera")
;; #S(bibtex-name :first ((#\Space . "Jes{'u}s") (#\Space . "A.")
;;                        (#\Space . "{D}e"))
;;                :von nil
;;                :last ((#\Space . "Loera"))
;;                :jr nil)
;;; FIXME: Original BibTeX seems to recognize a "von" token here!?!  -FIXED.

(parse-bibtex-name "Jes{\'u}s A. De Loera")
;; #S(bibtex-name :first ((#\Space . "Jes{'u}s") (#\Space . "A.")
;;                        (#\Space . "De"))
;;                :von nil
;;                :last ((#\Space . "Loera"))
;;                :jr nil)
(parse-bibtex-name "Jes{\'u}s A. de Loera")
;; #S(bibtex-name :first ((#\Space . "Jes{'u}s") (#\Space . "A."))
;;                :von ((#\Space . "de"))
;;                :last ((#\Space . "Loera"))
;;                :jr nil)

(parse-bibtex-name "{\\relax{Ch}}ristos H. Papadimitriou")

||#

