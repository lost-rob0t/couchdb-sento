(in-package :couchdb-sento)

(defvar *msg* nil "Dynamic Var of incomming message for use inside define-couchdb-service.")

(defmacro define-couchdb-service ((agent system client &key (actor-name "couchdb") (dispatcher-id :shared)) &body body)
  "Create a couchdb actor that makes api calls to couchdb, the client is is wraped with client-var"
  (alexandria:with-gensyms (msg sender)
    `(ac:actor-of ,system :name ,actor-name
                          :receive (lambda (,msg)
                                     (tasks:with-context (,system ,dispatcher-id)
                                       (let* ((*msg* ,msg)
                                              (,sender act:*sender*)
                                              (fut (future:with-fut
                                                     (couchdb-agent:with-couchdb-agent (,client ,agent)
                                                       (flet ((get-arg (arg)
                                                                (getf *msg* arg)))
                                                         ,@body)))))
                                         (future:fcompleted fut (result)
                                           (act:reply result ,sender))))))))
(defun couch! (actor &rest args)
  "Send a message to a couchdb actor. they expect &keys style messages"
  (act:tell actor (apply #'list args)))

(defun couch? (actor timeout &rest args)
  "Send a message to a couchdb actor. they expect &keys style messages"
  (act:ask actor (apply #'list args) :time-out timeout))


;; Simple services
(defun make-get-service (agent system name)
  (define-couchdb-service (agent system client :actor-name name)
    (cl-couch:get-document client (get-arg :database) (get-arg :id))))

(defun make-insert-service (agent system name)
  "Couchdb Actor that will handle inserts"
  (define-couchdb-service (agent system client :actor-name name)
    (cl-couch:create-document client (get-arg :database) (get-arg :document))))

(defun make-bulk-insert-service (agent system name)
  "Couchdb Actor that will handle inserts"
  (define-couchdb-service (agent system client :actor-name name)
    (cl-couch:bulk-create-documents client (get-arg :database) (get-arg :documents))))

(defun make-bulk-get-service (agent system name)
  "Couchdb Actor that will handle inserts"
  (define-couchdb-service (agent system client :actor-name name)
    (cl-couch:bulk-get-documents client (get-arg :database) (get-arg :documents) :revs (if (get-arg :revs) "true" "false"))))
