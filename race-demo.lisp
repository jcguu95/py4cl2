;;; Demonstrates the race condition in py4cl2's *freed-python-objects*
;;;
;;; The bug: (push x *freed-python-objects*) is not atomic.
;;; It expands to (setq *freed-python-objects* (cons x *freed-python-objects*))
;;; which is: READ old value -> CONS new cell -> WRITE back.
;;;
;;; If two threads interleave at the WRITE step, one thread's cons cell
;;; gets overwritten and its handle is silently lost — that Python object
;;; leaks forever.

;;; ── Exact replica of the py4cl2 code ───────────────────────────────────────

(defvar *freed-python-objects* nil)

(defun free-python-object (python-id handle)
  ;; THIS IS THE BUGGY LINE from src/reader.lisp:31
  (push (list python-id handle) *freed-python-objects*))

;;; ── Reproduction ────────────────────────────────────────────────────────────

(defparameter +n-threads+ 8)
(defparameter +pushes-per-thread+ 50000)
(defparameter +expected+ (* +n-threads+ +pushes-per-thread+))

(defun run-race ()
  (setq *freed-python-objects* nil)
  (let ((threads
          (loop for tid from 0 below +n-threads+
                collect (sb-thread:make-thread
                          (lambda (id)
                            (loop for h from 0 below +pushes-per-thread+
                                  do (free-python-object 42 (+ (* id +pushes-per-thread+) h))))
                          :arguments (list tid)
                          :name (format nil "gc-thread-~d" tid)))))
    (dolist (th threads) (sb-thread:join-thread th)))
  (length *freed-python-objects*))

;;; ── Run it several times and show the damage ────────────────────────────────

(format t "~%Expected handles per run: ~d~%~%" +expected+)
(format t "~16@a  ~10@a  ~10@a~%" "Run" "Got" "Lost")
(format t "~16@a  ~10@a  ~10@a~%" "---" "---" "----")

(let ((total-lost 0))
  (loop for run from 1 to 5
        do (let* ((got  (run-race))
                  (lost (- +expected+ got)))
             (incf total-lost lost)
             (format t "~16d  ~10d  ~10d~%" run got lost)))
  (format t "~%Total handles lost across 5 runs: ~d~%" total-lost)
  (format t "~%(Each lost handle = a Python object that will never be freed = memory leak)~%~%"))
