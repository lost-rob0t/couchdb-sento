(in-package :couchdb-sento)

;; Only admin:password for local dev testing ie from docker
(defun make-couchdb-agent (context
                           &key (user "admin")
                             (password "password")
                             (host "localhost")
                             (port 5984)
                             (scheme "http")
                             (max-idle 2)
                             (max-cons 10))
  (make-agent (lambda ()
                (anypool:make-pool :name "couchdb-connections"
                                   :connector (lambda ()
                                                (let ((client (cl-couch:new-couchdb host port :scheme scheme)))
                                                  (cl-couch:password-auth client user password)
                                                  client))

                                   :max-idle-count max-idle
                                   :disconnector (lambda (obj)
                                                   (setf (cl-couch:couchdb-headers obj) nil))
                                   :max-open-count max-cons))))



(defun couchdb-agent-pool (agent)
  "Get the couchdb client pool for use with cl-couch"
  (agt:agent-get agent #'identity))

(defun pool-stats (agent)
  (let ((pool (couchdb-agent-pool agent)))
    (log:info "Active conns in pool: ~A" (anypool:pool-active-count pool))
    (log:info "IDLE: ~A" (anypool:pool-idle-count pool))))

(defmacro with-couchdb-agent ((client agent) &body body)
  `(let ((pool (couchdb-agent-pool ,agent)))
     (anypool:with-connection (,client pool)
       ,@body)))
