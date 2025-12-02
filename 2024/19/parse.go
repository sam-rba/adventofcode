package main

import (
	"bufio"
	"bytes"
	"io"
	"log"
)

func parse(in io.Reader) (towels []string, designs <-chan string, err error) {
	r := bufio.NewReader(in)

	towels, err = parseTowels(r)
	if err != nil {
		return nil, nil, err
	}

	_, err = r.ReadBytes('\n')
	if err != nil {
		return nil, nil, err
	}

	c := make(chan string)
	designs = c
	go parseDesigns(r, c)

	return
}

func parseTowels(r *bufio.Reader) ([]string, error) {
	line, err := r.ReadBytes('\n')
	if err != nil {
		return nil, err
	}
	line = bytes.TrimSuffix(line, []byte{'\n'})

	fields := bytes.Split(line, []byte(", "))
	towels := make([]string, len(fields))
	for i := range fields {
		towels[i] = string(fields[i])
	}
	return towels, nil
}

func parseDesigns(r *bufio.Reader, designs chan<- string) {
	defer close(designs)
	for {
		line, err := r.ReadBytes('\n')
		if err == io.EOF {
			return
		} else if err != nil {
			log.Fatal(err)
		}
		line = bytes.TrimSuffix(line, []byte{'\n'})
		if len(line) == 0 {
			return
		}
		designs <- string(line)
	}
}
