(asdf:defsystem :couchdb-sento
  :version      "0.1.0"
  :description  "Sento actors for couchdb. provides a service actor and a agent for managing state."
  :author       " <nsaspy@airmail.cc>"
  :serial       t
  :license      "MIT"
  :components   ((:file "source/package")
                 (:file "source/agent")
                 (:file "source/actor"))
  :depends-on   (#:sento :cl-couch :log4cl :anypool))
