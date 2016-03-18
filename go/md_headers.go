package main

// Scans through a Markdown document and converts any headers from:
//
//   # This is a header
//
// To:
//
//   This is a header
//   ----------------

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"regexp"
	"strings"
)

func main() {
	// Use -file to specify the name of the Markdown file.
	var filename string
	flag.StringVar(&filename, "file", "", "Name of a Markdown file")
	flag.Parse()

	if len(filename) == 0 {
		fmt.Println("Error: name of a Markdown file required")
		os.Exit(1)
	}

	// The regular expression that matches headers.
	re := regexp.MustCompile("^#[[:space:]](.+)$")

	// Open the Markdown file.
	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// Scan each line and convert headers.
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		// Is the line a header?
		matches := re.FindStringSubmatch(line)
		if len(matches) == 0 {
			// It's an ordinary line.
			fmt.Println(line)
			continue
		}

		// It's a header, convert it.
		header := matches[1]
		fmt.Println(header)
		fmt.Println(strings.Repeat("-", len(header)))
	}
}
