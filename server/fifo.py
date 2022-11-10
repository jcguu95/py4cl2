import os
import errno
from util import *

def gen_fifopath ():
    import uuid
    name = str(uuid.uuid1())
    fifo_dir = '/tmp/python-server/'
    fifopath = fifo_dir + name
    ensure_dir(fifo_dir)
    print(fifopath)
    return fifopath

def create_fifo (fifopath):
    try:
        os.mkfifo(fifopath)
        os.chmod(fifopath, 0o666)
    except OSError as oe:
        if oe.errno != errno.EEXIST:
            raise

def write_fifo (fifopath, content):
    with open(fifopath, 'w') as fifo:
        fifo.write(str(content))
        fifo.write("\n")

def remove_fifo (fifopath):
    os.remove(fifopath)
