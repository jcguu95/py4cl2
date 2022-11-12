import time
from threading import Thread

from fifo import *
from py2lisp import *

def work (request, fifopath):
    mode, content = request["mode"], request["content"]
    if mode == "eval":
        try:
            obj  = eval(content)
            dic  = lispify(obj)
            kind = dic["kind"]
            body = dic["body"]
        except Exception as e:
            kind = "error"
            body = lispify(e)["body"]
        msg = encode(kind, body)
    elif mode == "exec":
        try:
            exec(content)
            body = "T"
            kind = "eval"
        except Exception as e:
            kind = "error"
            body = lispify(e)["body"]
        msg = encode(kind, body)
    try:
        print("Sending message: ", msg, "\n")
        create_fifo(fifopath)
        write_fifo(fifopath, msg)
    finally:
        # TODO Find if I can wait for idle time for deletion.
        async_remove_fifo(fifopath,120)
    # request["final"] = time.time()
    # print("Finally, ", request)
    # print("Time Elaspe: ", request["final"] - request["start"])
    return request

def handle_request (request):
    fifopath = gen_fifopath()
    worker = Thread(target=work,
                    args=(request,fifopath))
    worker.start()
    return fifopath

# request_examples = \
#   [{"mode": "eval", "content": "1+1"},
#    {"mode": "eval", "content": "1/0"},
#    {"mode": "eval", "content": "8"},
#    {"mode": "exec",
#     "content": """
# def ggg (): return 7
# print(ggg())
# """}
# ]

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

# TODO Can't start too many threads. Need to check resources
# before giving permission for threads.
@app.post('/generic')
async def method_generic (data: Data):
    # start   = time.time()
    request = {"mode":    data.mode,
               "content": data.content,
               # "start":   start
               }
    # print(start, "Received request:", request)
    return handle_request(request)

DEFAULT_PORT=8787

def run_server ():
    uvicorn.run(app, host="0.0.0.0", port=DEFAULT_PORT)

if __name__ == "__main__":
    run_server()
