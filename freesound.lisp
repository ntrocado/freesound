;;;; freesound.lisp

(in-package #:freesound)

(defun system-relative-read-file (filename)
  (let ((file (asdf:system-relative-pathname "freesound" filename)))
    (when (uiop:file-exists-p file)
      (uiop:read-file-string file))))

(defvar *token*
  ;; try to read the token from a ".token" file in the same directory as this source code
  (system-relative-read-file ".token")
  "An alphanumeric string issued by Freesound to authenticate API calls.")

(defvar *client-id*
  ;; try to read to client-id from a ".client-id" file in the same directory as this source code
  (system-relative-read-file ".client-id")
  "An alphanumeric string issued by Freesound, used as part of OAuth2 authentication.")

(defvar *oauth2-access-token* nil
  "An alphanumeric string used as an access token for OAuth2 restricted resources. Call `oauth2-get-tokens` to initialize.")
(defvar *oauth2-refresh-token* nil
  "An alphanumeric string used to get a new OAuth2 access token, without starting the whole authentication process. Call `oauth2-get-tokens` to initialize. To refresh the OAuth2 access token, pass this to `oauth2-get-tokens` with `:refresh t`.")

(defparameter *root-uri* "https://freesound.org/")

(defun uri (resource &optional (root *root-uri*))
  (uiop:strcat root resource))

(defun oauth2-authorize (&optional (client-id *client-id*))
  "As the first step of OAuth2 authentication, open the default browser on a Freesound page, where users are prompted to log in and asked to give permission for the application. The url is also printed to standard output."
  (check-type client-id string)
  (let ((url (format nil
	   "~a?client_id=~a&response_type=code"
	   (uri "apiv2/oauth2/authorize/")
	   client-id)))
    (print url)
    (finish-output)
    (trivial-open-browser:open-browser url)))

(defun oauth2-get-tokens (code &key (token *token*) (client-id *client-id*) (refresh nil))
  "Return the OAuth2 access token and a refresh token. CODE is either the initial authorization code, or a previously generated refresh token. In this second case, REFRESH must be T. Also set *oauth2-access-token* to the new value."
  (let* ((params (if refresh
		     "refresh_token&refresh_token="
		     "authorization_code&code="))
	 (response
	   (yason:parse
	    (dex:post
	     (format nil
       		     "~a?client_id=~a&client_secret=~a&grant_type=~a~a"
		     (uri "apiv2/oauth2/access_token/")
		     client-id
		     token
		     params
		     code))))
	 (access-token (gethash "access_token" response))
	 (refresh-token (gethash "refresh_token" response)))
    (setf *oauth2-access-token* access-token
	  *oauth2-refresh-token* refresh-token)
    (values access-token refresh-token)))

(defun resource (uri &key (method :get) content (authentication :token)
		       (token *token*) (oauth2-access-token *oauth2-access-token*))
  (let ((header (list (cons "Authorization"
			    (ecase authentication
			      (:token (progn (check-type token string)
					     (uiop:strcat "Token " token)))
			      (:oauth2 (progn (check-type oauth2-access-token string)
					      (uiop:strcat "Bearer " oauth2-access-token))))))))
    (yason:parse (dex:request uri :method method :content content :headers header))))

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

;;; Search resources

(defun parse-filter (filter)
  (when filter
    (etypecase filter
      (list
       (string-trim '(#\Space)
		    (with-output-to-string (out)
		      (dolist (arg (ensure-list-of-lists filter))
			(assert (= (length arg) 2))
			(destructuring-bind (field val)
			    arg
			  (format out "~a:"
				  (etypecase field
				    (string field)
				    (symbol (string-downcase (symbol-name field)))))
			  (if (listp val)
			      (ecase (first val)
				(:and (format out "(~{~a~^ AND ~}) " (rest val)))
				(:or (format out "(~{~a~^ OR ~}) " (rest val)))
				(:range (format out "[~a TO ~a] " (cadr val) (caddr val)))
				(:range-to (format out "[* TO ~a] " (second val)))
				(:range-from (format out "[~a TO *] " (second val))))
			      (format out "~a " val)))))))
      (string filter))))

(defun text-search (query &key filter sort group-by-pack
			    page page-size fields descriptors normalized)
  "Search sounds by matching their tags and other kids of metadata, returning a hash-table with the sound list response.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id53

FILTER is either a string conforming to the API syntax, or a list of key/value pairs. For example:
(text-search \"trumpet\" :filter '((:type \"ogg\") (:channels 2)))

Each filter property can be further specified with the operators :and, :or, :range, :range-to, and :range-from. For example:
(text-search \"rain\" 
             :filter '((:tag (:and \"soundscape\" \"forest\")) 
                       (:created (:range-from \"2010-12-01T23:59:59.999Z\")) 
                       (:duration (:range 10 120))))"
  (resource
   (uiop:strcat (uri "apiv2/search/text/")
		(http-parameters (list "query" (format nil "~{~a~^ ~}"
						       (alexandria:ensure-list query))
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
  "Search sounds based on their content descriptors, returning a hash-table with the sound list response.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id54

The filter syntax described in `text-search` also applies to DESCRIPTORS-FILTER."
  (resource
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
  "Perform a combination of text search and content search, returning a hash-table with the sound list response.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id55

See `text-search` and `content-search`."
  (assert (not (or (eq (null query) (null filter))
		   (when query (null descriptors-filter))
		   (when target (null filter))))
	  (query target filter descriptors-filter)
	  "You must specify either a query or a target parameter (but not both), and at least one text-based or content-based filter (filter and descriptors-filter). In any case, you must always use at least one text-based search request parameter and one content-based search request parameter. Parameters given were: query = ~a; target = ~a; filter = ~a; descriptors-filter= ~a."
	  query target filter descriptors-filter)
  (resource
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

;;; Sound resources

(defun info (sound-id &key fields descriptors normalized)
  "Retrieve information about SOUND-ID.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id57"
  (resource
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
  "Download a lossy version of SOUND-ID, writing it to PATHNAME. FORMAT is either :hq-mp3, :hq-ogg, :lq-mp3, :lq-ogg, where \"hq\" is higher quality and \"lq\" lower quality."
  (dex:fetch (gethash (translate-preview-format format)
		      (gethash "previews"
			       (info sound-id :fields "previews")))
	     pathname))

(defun analysis (sound-id &key descriptors normalized)
  "Retrieve analysis information (content-based descriptors) on SOUND-ID.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id58"
  (resource
   (uri (format nil "apiv2/sounds/~a/analysis/~@[~a~]"
		sound-id
		(http-parameters (list "descriptors" (ensure-commas descriptors)
				       "normalized" normalized))))))

(defun similar (sound-id &key descriptors-filter
			   page page-size fields descriptors normalized)
  "Retrieve sounds similar to SOUND-ID, returning a hash table with the sound list response.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id59

The filter syntax described in `text-search` also applies to DESCRIPTORS-FILTER."
  (resource
   (uri (format nil "apiv2/sounds/~a/similar/~@[~a~]"
		sound-id
		(http-parameters (list "descriptors_filter" (parse-filter descriptors-filter)
				       "page" page
				       "page_size" page-size
				       "fields" (ensure-commas fields)
				       "descriptors" (ensure-commas descriptors)
				       "normalized" normalized))))))

(defun comments (sound-id &key page page-size)
  "Retrieves comments for SOUND-ID.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id60"
  (resource (uri (format nil "apiv2/sounds/~a/comments/~@[~a~]"
			     sound-id
			     (http-parameters (list "page" page
						    "page_size" page-size))))))

(defun download (sound-id pathname &key (if-exists :supersede) (if-does-not-exist :create))
  "Download SOUND-ID into PATHNAME. Keyword options are as in `open`. OAuth2 required.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id61"
  (check-type *oauth2-access-token* string)
  (with-open-file (out pathname
		       :direction :output :element-type '(unsigned-byte 8)
		       :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (alexandria:copy-stream (dex:get (uri (format nil "apiv2/sounds/~a/download/" sound-id))
				     :headers (list (cons "Authorization"
							  (uiop:strcat "Bearer "
								       *oauth2-access-token*)))
				     :want-stream t :force-binary t)
			    out)))

(defun optional-params-list (names params)
  (assert (= (length names) (length params)))
  (loop :for param :in params
	:for name :in names
	:when param
	  :collect (cons name param)))

(defun description-tags-format (tags)
  (when tags
    (etypecase tags
      (string tags)
      (list (format nil "~{~(~a~)~^ ~}" tags)))))

(defun translate-license (license)
  (when license
    (if (stringp license)
	license
	(ecase license
	  (:attribution "Attribution")
	  (:attribution-noncommercial "Attribution Noncommercial")
	  (:creative-commons "Creative Commons 0")))))

(defun upload (file &key name tags description license pack geotag)
  "Upload an audio FILE into Freesound and (optionally) describe it. If a description is intended, all of TAGS, DESCRIPTION and LICENSE are required. OAuth2 required.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id62

LICENSE is either one of the strings accepted by the API, or one of the following: :attribution, :attribution-noncommercial, :creative-commons."
  (assert (or (and tags description license)
  	      (not (or tags description license)))
  	  ()
  	  "If either one of :tags, :description or :license is given, all of them are required.")
  (resource (uri "apiv2/sounds/upload/")
	    :method :post
	    :content (optional-params-list
		      '("audiofile" "name" "tags" "description" "license" "pack" "geotag")
		      (list (uiop:ensure-pathname file)
			    name (description-tags-format tags) description
			    (translate-license license) pack geotag))
	    :authentication :oauth2))

(defun describe-sound (upload-filename tags description license &key name pack geotag)
  "Describe a previously uploaded audio file that has not yet been described. OAuth2 required.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id63

LICENSE is either one of the strings accepted by the API, or one of the following: :attribution, :attribution-noncommercial, :creative-commons."
  (resource (uri "apiv2/sounds/describe/")
	    :method :post
	    :content (optional-params-list
		      '("upload_filename" "name" "tags" "description" "license" "pack" "geotag")
		      (list upload-filename name (description-tags-format tags) description
			    (translate-license license) pack geotag))
	    :authentication :oauth2))

(defun pending-uploads ()
  "Retrieve a list of audio files uploaded by the Freesound user logged in using OAuth2 that have not yet been described, processed or moderated.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id64"
  (resource (uri "apiv2/sounds/pending_uploads/") :authentication :oauth2))

(defun edit-sound-description (sound-id &key name tags description license pack geotag)
  "Edit the description of an already existing sound. Note that this resource can only be used to edit descriptions of sounds created by the Freesound user logged in using OAuth2.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id65

LICENSE is either one of the strings accepted by the API, or one of the following: :attribution, :attribution-noncommercial, :creative-commons."
  (resource (uri "apiv2/sounds/~a/edit/" sound-id)
	    :method :post
	    :content (optional-params-list
		      '("name" "tags" "description" "license" "pack" "geotag")
		      (list name (description-tags-format tags) description
			    (translate-license license) pack geotag))
	    :authentication :oauth2))

(defun bookmark (sound-id &key name category)
  "Bookmark an existing sound SOUND-ID. The sound will be bookmarked by the Freesound user logged in using OAuth2, therefore this method requires OAuth2 authentication.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id66"
  (resource (uri (format nil "apiv2/sounds/~a/bookmark/" sound-id))
	    :method :post :content `(("name" . ,name)
				     ("category" . ,category))
	    :authentication :oauth2))

(defun rate (sound-id rating)
  "Rate an existing sound SOUND-ID with RATING, between 0 and 5 (where 5 is the maximum). The sound will be rated by the Freesound user logged in using OAuth2.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id67"
  (assert (<= 0 rating 5))
  (resource (uri (format nil "apiv2/sounds/~a/rate/" sound-id))
	    :method :post :content `(("rating" . ,rating))
	    :authentication :oauth2))

(defun comment (sound-id comment)
  "Post a COMMENT to an existing sound SOUND-ID. The comment will appear to be made by the Freesound user logged in using OAuth2.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id68"
  (resource (uri (format nil "apiv2/sounds/~a/comment" sound-id))
	    :method :post :content `(("comment" . ,comment))
	    :authentication :oauth2))

;;; User resources

(defun user-info (username)
  "Retrieve information about Freesound user USERNAME.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id70"
  (resource (uri (format nil "apiv2/users/~a/" username))))

(defun user-sounds (username &key page page-size fields descriptors normalized)
  "Retrieve information about the sounds uploaded by Freesound user USERNAME, returning a hash table with the sound list response.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id71"
  (resource (uri (format nil "apiv2/users/~a/sounds/~a"
			 username
			 (http-parameters (list "page" page
						"page_size" page-size
						"fields" (ensure-commas fields)
						"descriptors" (ensure-commas descriptors)
						"normalized" normalized))))))

(defun user-packs (username &key page page-size)
  "Retrieve information about the packs uploaded by Freesound user USERNAME, returning a hash table with the sound list response.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id72"
  (resource (uri (format nil "apiv2/users/~a/packs/~a"
			 username
			 (http-parameters (list "page" page
						"page_size" page-size))))))

(defun user-bookmark-categories (username &key page page-size)
  "Retrieve the bookmark categories uploaded by Freesound user USERNAME, returning a hash table with the sound list response.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id73"
  (resource (uri (format nil "apiv2/users/~a/bookmark_categories/~a"
			 username
			 (http-parameters (list "page" page
						"page_size" page-size))))))

(defun user-bookmark-category-sounds (username &key (bookmark-category-id 0)
						 page page-size fields descriptors normalized)
  "Retrieve information about the sounds from a bookmark category created by Freesound user USERNAME, returning a hash table with the sound list response.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id74"
  (resource (uri (format nil "apiv2/users/~a/bookmark_categories/~a/sounds/~a"
			 username
			 bookmark-category-id
			 (http-parameters (list "page" page
						"page_size" page-size
						"fields" (ensure-commas fields)
						"descriptors" (ensure-commas descriptors)
						"normalized" normalized))))))

;;; Pack resources

(defun pack-instance (pack-id)
  "Retrieve information about the pack PACK-ID.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id76"
  (resource (uri (format nil "apiv2/packs/~a/" pack-id))))

(defun pack-sounds (pack-id &key page page-size fields descriptors normalized)
  "Retrieve information about the sounds included in the pack PACK-ID.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id77"
  (resource (uri (format nil "apiv2/packs/~a/sounds/~a"
			 pack-id
			 (http-parameters (list "page" page
						"page_size" page-size
						"fields" (ensure-commas fields)
						"descriptors" (ensure-commas descriptors)
						"normalized" normalized))))))

(defun pack-download (pack-id pathname &key (if-exists :supersede) (if-does-not-exist :create))
  "Download PACK-ID into PATHNAME. Keyword options are as in `open`. OAuth2 required.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id78"
  (check-type *oauth2-access-token* string)
  (with-open-file (out pathname
		       :direction :output :element-type '(unsigned-byte 8)
		       :if-exists if-exists :if-does-not-exist if-does-not-exist)
    (alexandria:copy-stream (dex:get (uri (format nil "apiv2/packs/~a/download/" pack-id))
				     :headers (list (cons "Authorization"
							  (uiop:strcat "Bearer "
								       *oauth2-access-token*)))
				     :want-stream t :force-binary t)
			    out)))

;;; Other resources

(defun me ()
  "Information about the user that is logged in using the OAuth2 procedure.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id80"
  (resource (uri "apiv2/me/") :authentication :oauth2))

(defun descriptors ()
  "Information about the available audio descriptors that are extracted from Freesound sounds.

API documentation: https://freesound.org/docs/api/resources_apiv2.html#id81"
  (resource (uri "apiv2/descriptors/")))

;;; Convenience functions

(defun print-columns (k v level stream)
  (format stream "~0,vt~a:" (* 15 level) (subseq k 0 (min 13 (length k))))
  (if (hash-table-p v)
      (maphash (lambda (k v) (print-columns k v (1+ level) stream)) v)
      (format stream "~15,15t~a~%" v)))

(defun print-info (info &optional (stream *standard-output*))
  "Pretty print INFO, which is the response to `info` (sound instance) or to `analysis`."
  (maphash (lambda (k v) (print-columns k v 0 stream)) info)
  info)

(defun print-search-result (search-result &optional (stream *standard-output*))
  "Pretty print SEARCH-RESULT, a sound list response."
  (assert (every (lambda (x) (nth-value 1 (gethash x search-result)))
		 '("previous" "results" "next" "count"))
	  (search-result)
	  "~a is not a hash table containing the response to a search query." search-result)
  (format t "Showing ~a results from a total of ~a:~%"
	  (length (gethash "results" search-result))
	  (gethash "count" search-result))
  (dolist (sound (gethash "results" search-result) search-result)
    (terpri)
    (print-info sound stream)))
