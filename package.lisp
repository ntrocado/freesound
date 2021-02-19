;;;; package.lisp

(defpackage #:freesound
  (:use #:cl)
  (:export
   #:*token*
   #:*client-id*
   #:*oauth2-access-token*
   #:*oauth2-refresh-token*
   #:oauth2-authorize
   #:oauth2-access-token
   #:text-search
   #:content-search
   #:combined-search
   #:info
   #:preview
   #:analysis
   #:similar
   #:comments
   #:download
   #:upload
   #:describe-sound
   #:pending-uploads
   #:edit-sound-description
   #:bookmark
   #:rate
   #:comment
   #:user-instance
   #:user-sounds
   #:user-packs
   #:user-bookmark-categories
   #:user-bookmark-categories-sounds
   #:pack-instance
   #:pack-sounds
   #:pack-download
   #:me
   #:descriptors))
