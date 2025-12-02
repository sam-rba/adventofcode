package main

import (
	"bufio"
	"io"
	"log"
	"strconv"
)

func parse(in io.Reader, out chan<- int) {
	defer close(out)
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		n, err := strconv.Atoi(scanner.Text())
		if err != nil {
			log.Fatal(err)
		}
		out <- n
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
