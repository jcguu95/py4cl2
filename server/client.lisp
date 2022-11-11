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

(defun monitor-fifo (fifo-path)
  ;; Better to use file-notify by Shinmera.
  (loop until (probe-file fifo-path))
  ;; (format t "monitor-fifo here~%") (force-output) ; TODO use log
  (with-open-file (fifo fifo-path)
    ;; FIXME This problem is serious. It makes the following form breaks:
    ;; (loop for i from 0 to 1000
    ;;       do (lparallel:future (async-py-eval "1+1"))
    ;;          (format t ": ~a~%" i))
    ;;
    ;; FIXME Even though the FIFO is opened, if Lisp starts reading
    ;; before python is writing, then Lisp just hangs there.
    ;; (sleep 1)                     ; kludge
    ;; (loop until (listen fifo))
    ;; (format t "monitor-fifo here again~%") (force-output) ; TODO use log
    (let ((msg (uiop:read-file-string fifo)))
      (format t "Received message: ~a~%" msg) (force-output)
      msg)))

(with-open-file (fifo fifo-path)
  (sleep 1)
  (format t "monitor-fifo here again~%") (force-output) ; TODO use log
  (let ((msg (uiop:read-file-string fifo)))
    (format t "Received message: ~a~%" msg) (force-output)
    msg))

(defun async-py-eval-request (content)
  (let* ((fifo-path (read-from-string
                     (py-request "generic"
                                 (list "mode"        "eval"
                                       "content"      content))))
         (msg (monitor-fifo fifo-path)))
    msg))

(defun async-py-eval (content)
  (with ((msg (async-py-eval-request content))
         (((kind :kind) (body :body)) ? (decode msg)))
    (case kind
      (#\r (identity (read-from-string body)))
      (#\e (error 'pyerror :text body)))))

;; TODO Suppoer execution.
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



;; ZMQ
(defun server ()
  "Bind to socket and wait to receive a message.  After receipt,
  return the message \"OK\"."
  (zmq:with-context (ctx 1)
    (zmq:with-socket (socket ctx zmq:rep)
      (zmq:bind socket "tcp://lo:5555")
      (loop
       (let ((query (make-instance 'zmq:msg)))
         (zmq:recv socket query)
         (format t "Recieved query: '~A'~%"
                 (zmq:msg-data-as-string query) ))
       (zmq:send socket (make-instance 'zmq:msg :data "OK")) ))))
