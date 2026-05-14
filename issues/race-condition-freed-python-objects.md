# Race Condition in `*freed-python-objects*` → Python Object Leak

## Summary

`free-python-object` (called from GC finalizers) and `delete-freed-python-objects`
(called from the main thread) both touch `*freed-python-objects*` without any
synchronization. Under concurrent GC, the majority of handles are silently
dropped, causing permanent memory leaks on the Python side.

## Root Cause

`src/reader.lisp:31`:

```lisp
(defun free-python-object (python-id handle)
  (push (list python-id handle) *freed-python-objects*))
```

`push` is not atomic. It expands to:

```lisp
(setq *freed-python-objects* (cons (list python-id handle) *freed-python-objects*))
```

That is three steps: **READ** → **CONS** → **WRITE**. When two GC finalizer
threads interleave at the WRITE step, one thread's cons cell is overwritten and
its handle vanishes from the list. `delete-freed-python-objects` never sees it,
so `_py4cl_objects[handle]` on the Python side is never `del`-ed.

The same problem exists on `pop` in `delete-freed-python-objects`, where the
main thread races with any finalizer that fires mid-loop.

## Reproduction

```
nix-shell -p sbcl --run 'sbcl --noinform --non-interactive --load race-demo.lisp'
```

(`race-demo.lisp` is at the repo root — it is a self-contained minimal replica
of the buggy code, no py4cl2 dependency needed.)

### Observed result (8 threads × 50 000 pushes = 400 000 expected):

| Run | Got    | Lost       |
|-----|--------|------------|
| 1   | 95 121 | **304 879** |
| 2   | 94 300 | **305 700** |
| 3   | 75 259 | **324 741** |
| 4   | 87 559 | **312 441** |
| 5   | 79 784 | **320 216** |

~76–81 % of handles are lost per run under moderate thread contention.

## Impact

Every lost handle corresponds to a Python object stuck in `_py4cl_objects` with
its reference count never reaching zero. In long-running sessions that create
many remote Python objects (e.g. repeated `with-remote-objects` blocks), this
accumulates without bound.

## Fix

### Option A — portable, using bordeaux-threads

```lisp
(defvar *freed-python-objects-lock* (bt:make-lock "freed-python-objects"))

(defun free-python-object (python-id handle)
  (bt:with-lock-held (*freed-python-objects-lock*)
    (push (list python-id handle) *freed-python-objects*)))

(defun delete-freed-python-objects ()
  (loop
    (let ((id-handle (bt:with-lock-held (*freed-python-objects-lock*)
                       (pop *freed-python-objects*))))
      (unless id-handle (return))
      (destructuring-bind (python-id handle) id-handle
        (when (and (python-alive-p)
                   (= *current-python-process-id* python-id))
          (raw-pyexec "
try:
  del _py4cl_objects[" (%pythonize handle) "]
except:
  pass"))))))
```

### Option B — lock-free CAS (SBCL only)

```lisp
(defun free-python-object (python-id handle)
  (loop
    (let ((old *freed-python-objects*))
      (when (eq old (sb-ext:compare-and-swap
                      (symbol-value '*freed-python-objects*)
                      old
                      (cons (list python-id handle) old)))
        (return)))))
```

Option A is recommended: portable, readable, and the lock is never held for
more than a cons + a pop.

## Notes

- This must be resolved before any work that increases GC pressure on
  `python-object` structs (e.g. more aggressive use of remote objects, or
  raising `+n-threads+` in tests).
- The Python-side `__del__` on `LispCallbackObject` / `UnknownLispObject`
  sending `"d"` messages is a separate (smaller) issue: CPython's `__del__`
  ordering under cyclic GC is not guaranteed, but that is a Python-side
  concern and less urgent.
