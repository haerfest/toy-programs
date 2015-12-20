package main

import (
	"fmt"
	"log"
	"io/ioutil"
	"regexp"
	"strings"
)

func main() {
	re := regexp.MustCompile(`(.+)ex(.+)y`)

	bytes, err := ioutil.ReadFile("words.txt")
	if err != nil {
		log.Fatal(err)
	}

	words := strings.Split(string(bytes), "\n")
	for _, word := range words {
		matches := re.FindStringSubmatch(word)
		if matches != nil {
			fmt.Printf("%v: %v + ex + %v + y\n", matches[0], matches[1], matches[2])
		}
	}
}
	
