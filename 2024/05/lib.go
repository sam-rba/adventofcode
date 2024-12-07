package main

import (
	"bufio"
	"log"
	"strconv"
	"strings"
)

func parseUpdates(in *bufio.Scanner, updates chan<- []int) {
	defer close(updates)
	for in.Scan() {
		update, err := parseUpdate(in.Text())
		if err != nil {
			log.Fatal(err)
		}
		updates <- update
	}
}

func parseUpdate(s string) ([]int, error) {
	fields := strings.Split(s, ",")
	pages := make([]int, len(fields))
	for i := range fields {
		var err error
		pages[i], err = strconv.Atoi(fields[i])
		if err != nil {
			return nil, err
		}
	}
	return pages, nil
}

func extractMiddle(updates <-chan []int, middle chan<- int) {
	defer close(middle)
	for update := range updates {
		middle <- update[len(update)/2]
	}
}
