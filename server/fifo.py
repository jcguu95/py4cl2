import os
import errno
import time
import pathlib
import string
import random
from util import *
from threading import Thread

def random_string (N):
    """Generate a random string of length N composed with
    lowercases and digits."""
    return ''.join(random.choices(string.ascii_lowercase+string.digits, k=N))

def gen_fifopath ():
    name = random_string(10)
    fifo_dir = '/tmp/python-server/'
    fifopath = fifo_dir + name
    ensure_dir(fifo_dir)
    print(fifopath)
    return fifopath

def create_fifo (fifopath):
    if pathlib.Path(fifopath).exists():
        raise Exception
    try:
        os.mkfifo(fifopath)
        os.chmod(fifopath, 0o666)
    except OSError as oe:
        if oe.errno != errno.EEXIST:
            raise

def write_fifo (fifopath, content):
    while not pathlib.Path(fifopath).exists(): pass
    print("write_fifo: start writing!")
    with open(fifopath, 'w') as fifo:
        fifo.write(str(content))
        fifo.write("\n")

def remove_fifo (fifopath, delay):
    ""

    "Remove the FIFO after synchronously sleep for DELAY seconds."""
    time.sleep(delay)
    print("Removing fifo: {0}".format(fifopath))
    os.remove(fifopath)

def async_remove_fifo (fifopath, delay):
    remover = Thread(target=remove_fifo, args=(fifopath, delay))
    remover.start()
    return fifopath
