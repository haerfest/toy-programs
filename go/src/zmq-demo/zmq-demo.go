package main

import (
	"fmt"
	"flag"
	"log"
	zmq "github.com/alecthomas/gozmq"
)

type MessageProcessor func(topic string, message []byte)

func connect(endpoint string) (*zmq.Context, *zmq.Socket) {
	context, _ := zmq.NewContext()
	socket, _ := context.NewSocket(zmq.SUB)
	socket.Connect(endpoint)
	socket.SetSubscribe("")

	return context, socket
}

func record(endpoint string, processor MessageProcessor) {
	context, socket := connect(endpoint)
	defer socket.Close()
	defer context.Close()

	for {
		topic, err := socket.Recv(0)
		if err != nil {
			panic(err)
		}

		if more, err := socket.RcvMore(); more && err == nil {
			message, err := socket.Recv(0)
			if err != nil {
				panic(err)
			}

			processor(string(topic), message)
		} else {
			processor(string(topic), nil)
		}
	}
}

func main() {
	var host = flag.String("host", "tcp://127.0.0.1:1234", "host to connect to")
	flag.Parse()

	if *host == "" {
		log.Fatal("No host specified")
	}

	go record(*host, func(t string, m []byte) {
		fmt.Printf("%v: %v bytes\n", t, len(m))
	})
	
	fmt.Println("Press ENTER to stop...")
	var input string
	fmt.Scanf("%s", &input)
}
	














