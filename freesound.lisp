;;;; freesound.lisp

(in-package #:freesound)

(defparameter *token*
  (let ((file (asdf:system-relative-pathname "freesound" ".token")))
    (if (uiop:file-exists-p file)
	(uiop:read-file-string file)
	;; paste your token here:
	"")))

(defparameter *root-uri* "https://freesound.org/")

(defun get-response (uri &optional (token *token*))
  (yason:parse
   (dex:get uri :headers (list (cons "Authorization"
				     (uiop:strcat "Token " token))))))

(defun uri (resource)
  (check-type *root-uri* string)
  (uiop:strcat *root-uri* resource))

(defun commas (lst)
  (format nil "~(~{~a~^,~}~)" lst))

(defun prepare-plist (plist)
  (let ((accum))
    (alexandria:doplist (key val plist (reverse accum))
      (when (and key val)
	(push key accum)
	(push (if (listp val)
		  (commas val)
		  val)
	      accum)))))

(defun http-parameters (plist)
  (format nil "?~(~{~a=~a~^&~}~)"
	  (prepare-plist plist)))

(defun text-search (query &key filter sort group-by-pack
			    page page-size fields descriptors normalized)
  (get-response (uiop:strcat (uri "apiv2/search/text/")
			     (http-parameters (list :query query
						    :filter filter
						    :sort sort
						    :group_by_pack group-by-pack
						    :page page
						    :page_size page-size
						    :fields fields
						    :descriptors descriptors
						    :normalized normalized)))))

(defun content-search ())

(defun combined-search ())

(defun info (sound-id &key fields descriptors normalized)
  (get-response (uri (format nil "apiv2/sounds/~a/~@[~a~]"
			     sound-id
			     (http-parameters (list :fields fields
						    :descriptors descriptors
						    :normalized normalized))))))

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

(defun analysis ())

(defun similar ())

(defun comments ())

(defun descriptors ()
  (get-response (uri "apiv2/descriptors/")))
