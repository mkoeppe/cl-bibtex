;;; A BibTeX re-implementation in Common Lisp - runtime package
;;; Copr. 2001, 2002 Matthias Koeppe <mkoeppe@mail.math.uni-magdeburg.de>
;;; This is free software, licensed under GNU GPL (see file COPYING)

(defpackage bibtex-runtime
  (:use "COMMON-LISP")
  (:export "*BIB-MACROS*" "*BIB-DATABASE*" "*BIB-ENTRIES*"
	   "*BIB-ENTRY*" "*BIB-PREAMBLE*" "*BIB-STYLE*"
	   "*BIB-FILES*" "*CITE-ALL-ENTRIES*" "*CITE-KEYS*"
	   "READ-AUX-FILE" "READ-BIB-DATABASE" "CITED-BIB-ENTRIES"
	   "BIBTEX-NAME" "MAKE-BIBTEX-NAME" "BIBTEX-NAME-FIRST"
	   "BIBTEX-NAME-VON" "BIBTEX-NAME-LAST" "BIBTEX-NAME-JR"
	   "*BIBTEX-SPLIT-INITIALS*" "PARSE-BIBTEX-NAME" "PARSE-BIBTEX-NAME-LIST"
	   "*BIBTEX-LONG-TOKEN-LENGTH*" "*BIBTEX-LONG-NAME-LENGTH*"
	   "FORMAT-BIBTEX-NAME"
	   "BIB-WARN"
	   "ADD-PERIOD-UNLESS-SENTENCE-END"
	   "WHITESPACE-P" "EMPTY-FIELD-P" "BIBTEX-STRING-PURIFY"
	   "BIBTEX-STRING-DOWNCASE" "BIBTEX-STRING-UPCASE" "BIBTEX-STRING-TITLEDOWNCASE"
	   "BIBTEX-STRING-PREFIX"
	   "BIB-WARN" "BIB-ERRROR" "BIB-FATAL"
	   #:*err-count* #:*history*
	   #:+spotless-history+ #:+warning-message+ #:+error-message #:+fatal-message 
	   #:*bbl-output*))

(in-package bibtex-runtime)

;;; Implementation dependencies

;; SBCL does not like (COPY-READTABLE NIL)

;; CMUCL 3.0.8 18c+ does not like (PEEK-CHAR T STREAM NIL #\Space); it won't skip
;; over whitespace when the EOF character is whitespace?

;;; Error history

(defconstant +spotless-history+ 0)
(defconstant +warning-message+ 1)
(defconstant +error-message+ 2)
(defconstant +fatal-message+ 3)

(defvar *history* +spotless-history+)
(defvar *err-count* 0)

(defun mark-history (level)
  (cond ((> level *history*)
	 (setq *history* level))
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
  (apply #'format *error-output* format-control args)
  (terpri *error-output*)
  (mark-warning))

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

;;; Reading the database files

(defvar *bib-stream* nil)
(defvar *bib-macros* nil "A hashtable associating macro names with their definitions")
(defvar *bib-database* nil "A hashtable associating BibTeX keys with entries")
(defvar *bib-entries* nil "A sequence containing all requested BibTeX entries")
(defvar *bib-preamble* "" "A string accumulating all BibTeX @PREAMBLEs")
(defvar *bib-entry* nil)

(defun read-bib-identifier ()
  "Read an identifier from *BIB-STREAM*, returning it as a string, or
nil if no identifier could be read."
  (if (digit-char-p (peek-char t *bib-stream*))
      nil
      (let ((s (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
	(loop as char = (peek-char nil *bib-stream*)
	      until (or (whitespace-p char)
			(member char '(#\" #\# #\% #\' #\( #\) #\, #\= #\{ #\} )))
	      do (vector-push-extend (read-char *bib-stream*) s))
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
	(chars '()))
    (loop (let ((char (read-char stream nil nil)))
	    (cond
	      ((and (char= char right-delimiter)
		    (zerop brace-level))
	       (return (coerce (reverse chars) 'string)))
	      ((char= char #\{)
	       (incf brace-level))
	      ((char= char #\})
	       (if (zerop brace-level)
		   (bib-warn "Unbalanced braces")
		   (decf brace-level))))
	    (setq chars (cons char chars))))))

(defun read-bib-field-token ()
  (case (peek-char t *bib-stream*)
    ((#\{)
     (read-char *bib-stream*)
     (scan-balanced-braces *bib-stream* #\}))
    ((#\")
     (read-char *bib-stream*)
     (scan-balanced-braces *bib-stream* #\"))
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
	 (when (null definition)
	   (bib-error "Macro ~A undefined" macro))
	 definition)))))

(defconstant +bib-whitespace-character-list+
  '(#\Newline #\Space #\Linefeed #\Return #\Page))

(defconstant +bib-sep-character-list+
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
  (setf *bib-preamble*
	(concatenate 'string *bib-preamble* 
		     (read-bib-field-value nil))))

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
    (unless (char= (peek-char t *bib-stream*) #\,)
      (bib-error "Expected `,' character"))
    (read-char *bib-stream*)
    (let ((entry (make-hash-table :size 16 :test 'equalp)))
      (setf (gethash "entry-type" entry) entry-type)
      (setf (gethash "key" entry) key) 
      (loop (multiple-value-bind (name value)
		(read-bib-field t)
	      (setf (gethash name entry)
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

;;; Computing the cited entries

(defun cited-bib-entries (cite-keys &key
			  (min-crossrefs 2))
  "Return a vector of the entries in *BIB-DATABASE* whose keys are
given in the list CITE-KEYS (if CITE-KEYS is the symbol T, return a
vector of all database entries.  When a crossref'd entry is referenced
at least :MIN-CROSSREFS times, it is included as a separate entry as
well."
  (declare (ignore min-crossrefs))
  (let ((bib-entries (make-array 0 :adjustable t :fill-pointer 0)))
    (cond
      ((eql cite-keys t)
       (loop for entry being each hash-value in *bib-database*
	     do (vector-push-extend entry bib-entries)))
      (t
       (dolist (key cite-keys)
	 (let ((entry (gethash key *bib-database*)))
	   (if entry
	       (vector-push-extend entry bib-entries)
	       (bib-warn "I didn't find a database entry for ~A"
			  key))))
       ;; do the crossrefs
;;       (loop for index from 0 below (length bib-entries)
;;             do (let* ((entry (aref entries index))
;;                       (crossref (gethash 'crossref entry)))
;;                  (when crossref
;;                    (let ((cross-entry (
		      ))
    bib-entries))

;;; BibTeX names

(defstruct bibtex-name 
  "BibTeX name, broken down into its components. Each component is a
list of name tokens (SEP-CHAR TOKEN-STRING)."
  first von last jr)

(defvar *bibtex-split-initials* nil
  "If non-nil, BibTeX understands that there are two abbreviated first
names in names like `Padberg, M.W.'.  The original BibTeX 0.99c,
written by Oren Patashnik in the WEB language, thinks that `M.W.' is a
single first name; hence, in abbreviated format, the name becomes
`M. Padberg' instead of `M. W. Padberg'.  That's the reason why the
default value of this variable is nil.")

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
		token-start)
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
  ;; FIXME: The original BibTeX code also checks whether control
  ;; sequences leading to lower-case characters are used.
  (and (> (length (cdr token)) 0)
       (lower-case-p (char (cdr token) 0))))

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

(defun format-bibtex-name-component (stream tokens full inter-token-string)
  (do ((tokens tokens (cdr tokens))
       (token-index 0 (+ token-index 1))
       (token-short nil)
       (prev-token-short nil token-short))
      ((null tokens))
    (let ((token (car tokens)))
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
	     ((and (= token-index 1)
		   prev-token-short)
	      (princ #\~ stream))
	     (t
	      (princ #\Space stream))))))
      (cond
	(full
	 (princ (cdr token) stream)
	 (setq token-short
	       (not (enough-text-chars (cdr token)
				       *bibtex-long-token-length*))))
	(t ;; FIXME: Handle special characters here (415)
	 (princ (char (cdr token) 0) stream)
	 (setq token-short t))))))  

(defun format-bibtex-name (stream format-string bibtex-name)
  (let ((string-output nil)
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
			  (format-bibtex-name-component s tokens
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

;;; Reading the AUX files

(defvar *aux-file-commands*
  '(("citation" . aux-citation-command)
    ("bibdata" . aux-bibdata-command)
    ("bibstyle" . aux-bibstyle-command)
    ("@@input" . aux-input-command)))

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
  
(defun read-tex-control-sequence (stream &key (skip-whitespace t))
  "Read a TeX control sequence from STREAM, assuming that the escape
character (\\) has already been read.  In the case of a control word,
trailing whitespace is flushed if :SKIP-WHITESPACE is non-nil."
  (let ((char (read-char stream nil #\Space)))
    (if (alpha-char-p char)
	(let ((result (make-array 1 :element-type 'character
				  :fill-pointer t :adjustable t
				  :initial-element char)))
	  (loop for char = (peek-char nil stream nil #\Space)
		while (alpha-char-p char)
		do (vector-push-extend (read-char stream) result))
	  (when skip-whitespace
	    (peek-char t stream nil nil))
	  result)
	(string char))))

(defun get-aux-command-and-process ()
  "Read a TeX control sequence from *AUX-STREAM*.  If the sequence is
found in *AUX-FILE-COMMANDS*, call the associated function."
  (let* ((ctl (read-tex-control-sequence *aux-stream* :skip-whitespace nil))
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
	   
(defun read-aux-file-recursively (name)
  (let* ((full-name (kpathsea:find-file name))
	 (stream (and full-name
		      (open full-name :if-does-not-exist nil))))
    (unless full-name
      (bib-fatal "I couldn't open auxiliary file: ~S" name))
    (let ((*aux-stream* stream))
      (loop as char = (peek-char nil *aux-stream* nil nil)
	    while char
	    do (cond
		 ((char= char #\\)
		  (read-char *aux-stream*)
		  (get-aux-command-and-process))
		 (t (read-line *aux-stream*)))))))

(defun read-aux-file (name)
  "Read an AUX file, modifying *CITE-KEYS*, *CITE-ALL-ENTRIES*,
*BIB-ENTRIES*, *BIB-FILES*, and *BIB-STYLE*."
  (let ((*citation-seen-p* nil)
	(*bibdata-seen-p* nil))
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
  "Process an AUX-file \\@@input command."
  (unless (char= (read-char *aux-stream* nil #\Space) #\{)
    (aux-error "Expected `{'"))
  (let ((file-name (scan-balanced-braces *aux-stream* #\})))
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
	  do (vector-push-extend (read-char stream) s))
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

(defvar *bbl-output* nil "The stream corresponding to the formatted bibliography (BBL) file.")
 

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

(defun bibtex-string-purify (string)
  "Remove nonalphanumeric characters except for whitespace and
sep-char characters (these get converted to a space) and removes
certain alphabetic characters contained in the control sequences
associated with a special character."
  ;; FIXME:
  string)

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

(defun write-tex-group (group &optional (stream *standard-output*))
  (dolist (token group)
    (etypecase token
      (character (princ token stream))
      (list (princ "{" stream)
	    (write-tex-group token stream)
	    (princ "}" stream))
      (string (princ "\\" stream)
	      (princ token stream)
	      (unless (and (= (length token) 1)
			   (not (alpha-char-p (char token 0))))
		(princ " " stream))))))

;;(write-tex-group (parse-tex-string "\\abc{\\.ef}"))

(defun tex-group-upcase (group)
  (mapcar (lambda (token)
	    (typecase token
	      (character (char-upcase token))
	      (t token)))
	  group))

(defun bibtex-string-upcase (string)
  "Convert to upper case all letters in STRING at brace-level 0."
  (with-output-to-string (s)
    (write-tex-group (tex-group-downcase (parse-tex-string string))
		     s)))

(defun tex-group-downcase (group)
  (mapcar (lambda (token)
	    (typecase token
	      (character (char-downcase token))
	      (t token)))
	  group))

(defun bibtex-string-downcase (string)
  "Convert to lower case all letters in STRING at brace-level 0."
  (with-output-to-string (s)
    (write-tex-group (tex-group-downcase (parse-tex-string string))
		     s)))

(defun tex-group-titledowncase (group)
  (let ((colon-state 'colon-and-whitespace))
    (mapcar (lambda (token)
	      (typecase token
		(character
		 (prog1
		     (if (eq colon-state 'colon-and-whitespace)
			 token
			 (char-downcase token))
		   (cond ((char= token #\:)
			  (setq colon-state 'colon))
			 ((and (whitespace-p token)
			       (eq colon-state 'colon))
			  (setq colon-state 'colon-and-whitespace))
			 (t
			  (setq colon-state nil)))))
		(t (setq colon-state nil)
		   token)))
	    group)))
				      
(defun bibtex-string-titledowncase (string)
  "Convert to lower case all letters except the very first character
in the STRING, which it leaves alone, and except the first character
following any `:' and then non-null whitespace, which it also leaves
alone.  Only those letters at brace-level 0 are affected."
  (with-output-to-string (s)
    (write-tex-group (tex-group-titledowncase (parse-tex-string string))
		     s)))

(defun bibtex-string-prefix (string num-tokens)
  (let ((group (parse-tex-string string)))
    (with-output-to-string (s)
      (write-tex-group 
       (subseq group 0 (min num-tokens (length group)))
       s))))

;;;

#|

(setq *bib-macros* (make-hash-table))
(setq *bib-database* (make-hash-table :test #'equalp))

#+ignore (with-input-from-string (s "@string{jan=\"Januar\"}
@string(feb={Febr{u}ar})@article{the-key, xyzzyz=\"Foo\"}")
  (read-bib-database s))

(with-open-file (s "/home/mkoeppe/cvs/iba-papers/iba-bib.bib")
  (read-bib-database s))

(let ((*bibtex-split-initials* t))
  (parse-bibtex-name-list (gethash 'editor (gethash "johnson-trick-96" *bib-database*))))

(defun show-author (id)
  (let ((*bibtex-split-initials* t))
    (parse-bibtex-name-list (gethash "author" (gethash id *bib-database*)))))

(let ((*bibtex-split-initials* t))
  (remove-duplicates
   (sort 
    (loop for entry being each hash-value in *bib-database*
	  appending (mapcar #'(lambda (name) (format-bibtex-name nil "{f.~}{vv~}{ll}{, jj}" name))
			    (parse-bibtex-name-list (gethash "author" entry))))
    #'string<=)
   :test #'equal))
   
  

(format-bibtex-name t "{ff} {ll}" (first (show-author "schnorr-euchner94")))

(parse-bibtex-name-list '())

|#
