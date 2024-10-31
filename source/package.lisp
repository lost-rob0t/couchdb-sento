(uiop:define-package   :couchdb-sento
  (:nicknames :couchdb-agent)
  (:use       :cl :sento.agent :sento.actor-context :cl-couch)
  (:export  #:make-couchdb-agent
            #:couchdb-agent-pool
            #:with-couchdb-agent
            #:pool-stats
            #:define-couchdb-service
            #:make-bulk-get-service
            #:make-bulk-insert-service
            #:make-insert-service
            #:make-get-service
            #:couch?
            #:couch!)
  (:documentation "Basic Couchdb actor library for sento. very simple wrappers around anypool and sento agents/actors."))
