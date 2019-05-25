;;; Code to read from python process over a stream

(in-package :py4cl)

(defstruct python-object
  "A handle for a python object
which couldn't be translated into a Lisp value.
TYPE slot is the python type string
HANDLE slot is a unique key used to refer to a value in python."
  (type "" :type string)
  handle)

(defun make-python-object-finalize (&key (type "") handle)
    "Make a PYTHON-OBJECT struct with a finalizer.
This deletes the object from the dict store in python.

Uses trivial-garbage (public domain)
"
    (tg:finalize
     (make-python-object :type type
                         :handle handle)
     (let ((python-id *current-python-process-id*))
       (lambda () ; This function is called when the python-object is garbage collected
         (ignore-errors
           (if (and
                (python-alive-p) ; If not alive, python-exec will start python
                (= *current-python-process-id* python-id)) ; Python might have restarted
               (python-exec "
try:
  del _py4cl_objects[" handle "]
except:
  pass")))))))

(defun stream-read-string (stream)
  "Reads a string from a stream
Expects a line containing the number of chars following
e.g. '5~%hello'
Returns the string or nil on error
"
  (let ((nchars (parse-integer (read-line stream))))
    (with-output-to-string (str)
      (loop for i from 1 to nchars do
           (write-char (read-char stream) str)))))

(defun stream-read-value (stream)
  "Get a value from a stream
Currently works by reading a string then using read-from-string
"
  (let ((str (stream-read-string stream)))
    (multiple-value-bind (value count)
        (read-from-string str)
      ;; Check if all characters were used
      (unless (eql count (length str))
        (error (concatenate 'string "unread characters in reading string \"" str "\""))) 
      value)))
