(ql:quickload :yason)
(ql:quickload :drakma)
(ql:quickload :lparallel)
(ql:quickload :local-time)
(setf lparallel:*kernel* (lparallel:make-kernel 5))

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

(defun async-py-eval (content)
  (let ((fifo-path nil))
    (setf fifo-path
          (read-from-string
           (py-request "generic"
                       (list "mode"        "eval"
                             "content"      content))))
    (read-from-string (monitor-fifo fifo-path))))

(defun monitor-fifo (fifo-path)
  ;; Better to use file-notify by Shinmera.
  (loop until (probe-file fifo-path))
  (with-open-file (fifo fifo-path)
    (loop until (listen fifo))
    (let ((msg (read-line fifo)))
      (progn (format t "Received: ~a~%" msg)
             (force-output))
      msg)))

(defun async-py-exec (content)
  (py-request "generic"
              (list "mode"        "exec"
                    "content"     content)))

(time
 (loop for i from 0 to 30
       do
            (py-request "pass"
                        (list "mode"        ""
                              "content"     ""))
            (format t "~a: ~a~%" i (local-time:now))
            (force-output)))
