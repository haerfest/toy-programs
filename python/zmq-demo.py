#!/usr/bin/env python3

import logging
import zmq

class Listener:
    def listen(self, addr, callback=None):
        context = zmq.Context.instance()
        socket  = context.socket(zmq.SUB)

        socket.connect(addr)
        socket.setsockopt(zmq.SUBSCRIBE, b'')
        
        while True:
            topic, message = socket.recv_multipart()
            if callback:
                callback(topic, message)

if __name__ == "__main__":
    def print_topic(topic, message):
        print(topic)

    Listener().listen("tcp://10.18.129.51:55555", print_topic)

            
        
