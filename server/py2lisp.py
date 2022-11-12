import traceback

python_to_lisp_type = {
    bool       : "BOOLEAN",
    type(None) : "NULL",
    int        : "INTEGER",
    float      : "FLOAT",
    complex    : "COMPLEX",
    list       : "VECTOR",
    dict       : "HASH-TABLE",
    str        : "STRING",
}

def dict_lispifier (dict):
    segment_  = "(setf (gethash (quote {}) table) (quote {}))"
    segments  = [segment_.format(lispify(key)["body"], lispify(value)["body"]) for key, value in dict.items()]
    segment_0 = "#.(let ((table (make-hash-table :test (quote equalp)))) "
    segment_1 = " ".join(segments)
    segment_2 = " table)"
    return segment_0 + segment_1 + segment_2

def tuple_lispifier (tuple):
    return "(quote (" + " ".join(lispify(elt)["body"] for elt in tuple) + "))"

def Exception_lispifier (exception):
    return "".join(traceback.format_exception(type(exception), exception, exception.__traceback__))

def float_lispifier (float):
    infnan = {"infd0" : "float-features:double-float-positive-infinity",
              "-infd0": "float-features:double-float-negative-infinity",
              "inf"   : "float-features:single-float-positive-infinity",
              "-inf"  : "float-features:single-float-negative-infinity",
              "nan"   : "float-features:single-float-nan",
              "nand0" : "float-features:double-float-nan"}
    if str(float).find("e") != -1:
        lispified_float = str(float).replace("e", "d")
    else:
        lispified_float = str(float)+"d0"
    if lispified_float in infnan:
        lispified_float = infnan[lispified_float]
    return lispified_float

lispifiers = {
    dict       : dict_lispifier,
    tuple      : tuple_lispifier,
    int        : str,
    float      : float_lispifier,
    complex    : lambda x: "#C(" + lispify(x.real)["body"] + " " + lispify(x.imag)["body"] + ")",
    bool       : lambda x: "T" if x else "NIL",
    type       : lambda x: "(quote " + python_to_lisp_type[x] + ")",
    list       : lambda x: "#(" + " ".join(lispify(elt)["body"] for elt in x) + ")",
    str        : lambda x: "\"" + x.replace("\\", "\\\\").replace("\"", "\\\"")  + "\"",
    type(None) : lambda x: "NIL",
    # Symbol     : str,
    # UnknownLispObject : lambda x: "#.(py4cl2::lisp-object {})".format(x.handle),
}

def lispify (obj):
    """
    Turn a python object into a string which can be parsed by Lisp reader.
    """
    if isinstance(obj, Exception):
        kind, body = "eval", Exception_lispifier(obj)
    elif type(obj) in lispifiers.keys():
        kind, body = "eval", lispifiers[type(obj)](obj)
    else:
        kind = "error"
        body = "Unsupported Object Type:\n  Object : {0}\n  Type   : {1}".format(str(obj), str(type(obj)))
    return {"kind": kind, "body": body}

def encode (kind, body):
    """Encode body into a message to be sent to Lisp."""
    match kind:
        case "eval":
            return "r"+body
        case "error":
            return "e"+body
        case _:
            raise Exception
