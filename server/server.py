import os
import errno
import time
from threading import Thread

def ensure_dir (dir):
    from pathlib import Path
    Path(dir).mkdir(parents=True, exist_ok=True)
    return dir

def random_fifo ():
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
    # print("write_fifo:", fifopath, str(content))
    with open(fifopath, 'w') as fifo:
        fifo.write(str(content))
        fifo.write("\n")
    # try:
    #     with open(fifopath, 'w') as fifo:
    #         fifo.write(str(content))
    #         fifo.flush()
    #         fifo.close()
    # except Exception as e:
    #     print("oh no.. exception:", e)

def remove_fifo (fifopath):
    os.remove(fifopath)

def do_work (request, fifopath):
    mode        = request["mode"]
    content     = request["content"]
    try:
        if   mode == "eval":
            result = py_to_lisp(eval(content))
        elif mode == "exec":
            exec(content)
            result = None
        # else: raise
    except:
        result = None
        print("Exceptions during eval/exec.", result)
    try:
        create_fifo(fifopath)
        write_fifo(fifopath, result)
    finally:
        print("Removing fifo: {0}".format(fifopath))
        remove_fifo(fifopath)

def handle_request (request):
    fifopath = random_fifo()
    worker = Thread(target=do_work,
                    args=(request,fifopath))
    worker.start()
    return fifopath

request_examples = \
  [{"mode": "eval",
    "content": "1+1"},

   {"mode": "eval",
    "content": "1/0"},

   {"mode": "eval",
    "content": "8"},

   {"mode": "exec",
    "content": """
def ggg (): return 7
print(ggg())
"""}]

### Server

import os
import uvicorn
from typing import List

from dataclasses import dataclass
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, root_validator
from starlette.responses import Response, JSONResponse
from starlette.status import HTTP_204_NO_CONTENT

app: FastAPI = FastAPI()

class Data(BaseModel):
    mode:        str
    content:     str

@app.post('/generic')
def method_generic (data: Data):
    print("Receiving data:", data)
    request = {"mode":        data.mode,
               "content":     data.content}
    return handle_request(request)

@app.post('/pass')
async def method_pass (data: Data):
    """To test how fast FastAPI can take requests."""
    pass

###

DEFAULT_PORT=8787

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=DEFAULT_PORT)
    # uvicorn.run("server:app", host="0.0.0.0", port=DEFAULT_PORT, workers=3)
