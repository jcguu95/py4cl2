import time
from threading import Thread

from fifo import *
from py2lisp import *

def work (request, fifopath):
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
    fifopath = gen_fifopath()
    worker = Thread(target=work, args=(request,fifopath))
    worker.start()
    return fifopath

# request_examples = \
#   [{"mode": "eval",
#     "content": "1+1"},

#    {"mode": "eval",
#     "content": "1/0"},

#    {"mode": "eval",
#     "content": "8"},

#    {"mode": "exec",
#     "content": """
# def ggg (): return 7
# print(ggg())
# """}]

####################
# Server
####################

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
async def method_generic (data: Data):
    print("Receiving data:", data)
    request = {"mode":        data.mode,
               "content":     data.content}
    return handle_request(request)

DEFAULT_PORT=8787

def run_server ():
    uvicorn.run(app, host="0.0.0.0", port=DEFAULT_PORT)

if __name__ == "__main__":
    run_server()
