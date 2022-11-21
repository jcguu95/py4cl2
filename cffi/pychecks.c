// Compile using: gcc -I/media/common-storage/miniconda3/include/python3.8 -c -Wall -Werror -fpic pychecks.c && gcc -shared -o libpychecks.so pychecks.o

#include <Python.h>

int PyCheck_Long(PyObject* o){return PyLong_Check(o);}

int PyCheck_Float(PyObject* o){return PyFloat_Check(o);}

int PyCheck_Unicode(PyObject* o){return PyUnicode_Check(o);}

int PyCheck_Tuple(PyObject* o){return PyTuple_Check(o);}

int PyCheck_List(PyObject* o){return PyList_Check(o);}

int PyCheck_Dict(PyObject* o){return PyDict_Check(o);}
