(ql:quickload :yason)
(ql:quickload :drakma)
(ql:quickload :local-time)
(ql:quickload :lparallel)
(setf lparallel:*kernel* (lparallel:make-kernel 5))
(ql:quickload :rutils)
(named-readtables:in-readtable rutils:rutils-readtable)
(import '(rutils:with rutils:?))

(defparameter *default-port* 8787)

(defmacro with-parsed-json-output (&body body)
  `(multiple-value-bind (result code etc uri thing bool ok)
       ,@body
     (declare (ignore code etc uri thing bool ok))
     (cond
       ((stringp result) result)
       (t (flexi-streams:octets-to-string
           result :external-format :utf-8)))))

(defun py-request (api plist)
  (with-parsed-json-output
    (drakma:http-request
     (format nil "http://~a:~D/~a" "localhost" *default-port* api)
     :method :post
     :external-format-out :utf-8
     :external-format-in :utf-8
     :additional-headers
     `(("Content-Type" . "application/json"))
     :content
     (with-output-to-string (s)
       (yason:encode
        (alexandria:plist-hash-table plist)
        s)))))

;; Better to use file-notify by Shinmera.
(defun monitor-fifo (fifo-path)
  (loop until (probe-file fifo-path))
  (let ((msg (uiop:read-file-string fifo-path)))
    ;; (format t "Received message: ~a" msg) (force-output)
    msg))

(defun async-py-request (mode content)
  (let* ((fifo-path
           (read-from-string
            (py-request "generic"
                        (list "mode"    mode
                              "content" content))))
         (msg (monitor-fifo fifo-path)))
    msg))

(defun async-py-eval (content)
  (with ((msg (async-py-request "eval" content))
         (((kind :kind) (body :body)) ? (decode msg)))
    (case kind
      (#\r (identity (read-from-string body)))
      (#\e (error 'pyerror :text body)))))

(defun async-py-exec (content)
  (with ((msg (async-py-request "exec" content))
         (((kind :kind) (body :body)) ? (decode msg)))
    (case kind
      (#\r (identity (read-from-string body)))
      (#\e (error 'pyerror :text body)))))

;; ;; TODO Support execution.
;; (defun async-py-exec (content)
;;   (py-request "generic"
;;               (list "mode"        "exec"
;;                     "content"     content)))

(defun decode (msg)
  "See `encode' on python side."
  (check-type msg string)
  (assert (> (length msg) 0))
  (let ((kind (elt msg 0))
        (body (subseq msg 1)))
    #h(:kind kind :body body)))

(define-condition pyerror (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "Python Error:~%~a" (text condition)))))



;; ;; TODO ZMQ
;; (defun server ()
;;   "Bind to socket and wait to receive a message.  After receipt,
;;   return the message \"OK\"."
;;   (zmq:with-context (ctx 1)
;;     (zmq:with-socket (socket ctx zmq:rep)
;;       (zmq:bind socket "tcp://lo:5555")
;;       (loop
;;        (let ((query (make-instance 'zmq:msg)))
;;          (zmq:recv socket query)
;;          (format t "Recieved query: '~A'~%"
;;                  (zmq:msg-data-as-string query) ))
;;        (zmq:send socket (make-instance 'zmq:msg :data "OK")) ))))
