;; services
(if (file-executable-p "cassandra")
  (prodigy-define-service
    :name "Cassandra"
    :command "cassandra"
    :args '("-f")
    :ready-message "Listening for thrift clients..."
    :tags '(einheit)
    :kill-process-buffer-on-stop t))

(if (file-executable-p "/opt/develop/java/zookeeper-3.4.6/bin/zkServer.sh")
  (prodigy-define-service
    :name "Zookeeper"
    :command "zkServer.sh"
    :args '("start-foreground")
    :cwd "/opt/develop/java/zookeeper-3.4.6"
    :path '("/opt/develop/java/zookeeper-3.4.6/bin")
    :ready-message "Starting zookeeper ... STARTED"
    :tags '(einheit)
    :kill-process-buffer-on-stop t))
