;;;; freesound.lisp

(in-package #:freesound)

(defparameter *token*
  (let ((file (asdf:system-relative-pathname "freesound" ".token")))
    (if (uiop:file-exists-p file)
	;; read the token from a ".token" file in the same directory as this source code
	(uiop:read-file-string file)
	;; or paste your token here:
	""))
  "An alphanumeric string issued by Freesound to authenticate API calls.")

(defparameter *root-uri* "https://freesound.org/")

(defun get-response (uri &optional (token *token*))
  (yason:parse
   (dex:get uri :headers (list (cons "Authorization"
				     (uiop:strcat "Token " token))))))

(defun uri (resource)
  (check-type *root-uri* string)
  (uiop:strcat *root-uri* resource))

(defun commas (lst)
  (format nil "~{~a~^,~}" lst))

(defun ensure-commas (in)
  (when in
    (etypecase in
      (list (commas in))
      ((or number string) in))))

(defun prepare-plist (plist)
  (let ((accum))
    (alexandria:doplist (key val plist (reverse accum))
      (when (and key val)
	(push key accum)
	(push (if (listp val)
		  (commas val)
		  val)
	      accum)))))

(defun spaces (string)
  (ppcre:regex-replace-all "\\s" string "%20"))

(defun http-parameters (plist)
  (spaces (format nil "?~{~a=~a~^&~}"
		  (prepare-plist plist))))

(defun ensure-list-of-lists (lst)
  (if (listp (first lst))
      lst
      (list lst)))

(defun parse-filter (filter)
  (etypecase filter
    (list
     (string-trim '(#\Space)
		  (with-output-to-string (out)
		    (dolist (arg (ensure-list-of-lists filter))
		      (assert (= (length arg) 2))
		      (destructuring-bind (field val)
			  arg
			(format out "~a:" field)
			(if (listp val)
			    (ecase (first val)
			      (:and (format out "(~{~a~^ AND ~}) " (rest val)))
			      (:or (format out "(~{~a~^ OR ~}) " (rest val)))
			      (:range (format out "[~a TO ~a] " (cadr val) (caddr val)))
			      (:range-to (format out "[* TO ~a] " (second val)))
			      (:range-from (format out "[~a TO *] " (second val))))
			    (format out "~a " val)))))))
    (string filter)))

(defun text-search (query &key filter sort group-by-pack
			    page page-size fields descriptors normalized)
  "Search sounds by matching their tags and other kids of metadata."
  (get-response
   (uiop:strcat (uri "apiv2/search/text/")
		(http-parameters (list "query" (ensure-commas query)
				       "filter" (parse-filter filter)
				       "sort" sort
				       "group_by_pack" group-by-pack
				       "page" page
				       "page_size" page-size
				       "fields" (ensure-commas fields)
				       "descriptors" (ensure-commas descriptors)
				       "normalized" normalized)))))

(defun parse-target (target)
  (etypecase target
    ((or string integer) target)
    (list (string-trim '(#\Space)
		       (with-output-to-string (out)
			 (dolist (arg (ensure-list-of-lists target))
			   (assert (and (= (length arg) 2)))
			   (format out
				   "~a:~d "
				   (first arg)
				   (ensure-commas (second arg)))))))))

;;; TODO add experimental analysis_file method
(defun content-search (target &key descriptors-filter 
				page page-size fields descriptors normalized)
  "Search sounds based on their content descriptors."
  (get-response
   (uiop:strcat (uri "apiv2/search/content/")
		(http-parameters (list "target" (parse-target target)
				       "descriptors_filter" (parse-filter descriptors-filter)
				       "page" page
				       "page_size" page-size
				       "fields" (ensure-commas fields)
				       "descriptors" (ensure-commas descriptors)
				       "normalized" normalized)))))

(defun combined-search (&key query filter sort target descriptors-filter
			  page page-size fields descriptors normalized)
  "Perform a combination of text search and content search."
  (assert (not (or (eq (null query) (null filter))
		   (when query (null descriptors-filter))
		   (when target (null filter)))))
  (get-response
   (uiop:strcat (uri "apiv2/search/combined/")
		(http-parameters (list "query" (ensure-commas query)
				       "filter" (parse-filter filter)
				       "sort" sort
				       "target" (parse-target target)
				       "descriptors_filter" (parse-filter descriptors-filter)
				       "page" page
				       "page_size" page-size
				       "fields" (ensure-commas fields)
				       "descriptors" (ensure-commas descriptors)
				       "normalized" normalized)))))

(defun result-plist (sound-list-response)
  (mapcar #'alexandria:hash-table-plist (gethash "results" sound-list-response)))

(defun info (sound-id &key fields descriptors normalized)
  (get-response
   (uri (format nil "apiv2/sounds/~a/~@[~a~]"
		sound-id
		(http-parameters (list "fields" (ensure-commas fields)
				       "descriptors" (ensure-commas descriptors)
				       "normalized" normalized))))))

(defun translate-preview-format (format)
  (ecase format
    (:hq-mp3 "preview-hq-mp3")
    (:hq-ogg "preview-hq-ogg")
    (:lq-mp3 "preview-lq-mp3")
    (:lq-ogg "preview-lq-ogg")))

(defun preview (sound-id pathname &key (format :hq-mp3))
  (dex:fetch (gethash (translate-preview-format format)
		      (gethash "previews"
			       (info sound-id :fields "previews")))
	     pathname))

(defun analysis (sound-id &key descriptors normalized)
  (get-response
   (uri (format nil "apiv2/sounds/~a/analysis/~@[~a~]"
		sound-id
		(http-parameters (list "descriptors" (ensure-commas descriptors)
				       "normalized" normalized))))))

(defun similar (sound-id &key descriptors-filter
			   page page-size fields descriptors normalized)
  (get-response
   (uri (format nil "apiv2/sounds/~a/similar/~@[~a~]"
		sound-id
		(http-parameters (list "descriptors_filter" (parse-filter descriptors-filter)
				       "page" page
				       "page_size" page-size
				       "fields" (ensure-commas fields)
				       "descriptors" (ensure-commas descriptors)
				       "normalized" normalized))))))

(defun comments (sound-id &key page page-size)
  "Retrieves comments for SOUND-ID."
  (get-response (uri (format nil "apiv2/sounds/~a/comments/~@[~a~]"
			     sound-id
			     (http-parameters (list "page" page
						    "page_size" page-size))))))

(defun descriptors ()
  (get-response (uri "apiv2/descriptors/")))
