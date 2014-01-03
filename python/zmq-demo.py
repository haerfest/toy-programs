#!/usr/bin/env python

import logging
import zmq

class Listener:
    def __init__(self, addr):
        self.addr = addr
        self.ctx  = zmq.Context.instance()
        self.sck  = self.ctx.socket(zmq.SUB)

    def listen(self, callback=None):
        self.sck.connect(self.addr)
        self.sck.setsockopt(zmq.SUBSCRIBE, b'')
        while True:
            parts = self.sck.recv_multipart()
            if len(parts) == 2:
                if callback:
                    callback(parts[0], parts[1])
            else:
                Logger.error("Received %d parts, but expected 2" % len(parts))

def foo(topic, msg):
    print topic

listener = Listener("tcp://10.18.129.51:55555")
listener.listen(foo)

            
        
