package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"slices"
)

func main() {
	i, err := findMarker(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%s: %d\n", part, i)
}

func findMarker(stream io.Reader) (int, error) {
	marker := make([]byte, 0, markerLen)
	r := bufio.NewReader(stream)
	for i := 1; ; i++ {
		b, err := r.ReadByte()
		if err != nil {
			return -1, err
		}
		marker = append(marker, b)
		if len(marker) == markerLen {
			if allDifferent(marker) {
				fmt.Println(string(marker))
				return i, nil
			}
			marker = marker[1:]
		}
	}
	return -1, fmt.Errorf("no marker")
}

func allDifferent(s []byte) bool {
	for i := range s {
		if slices.Contains(s[i+1:], s[i]) {
			return false
		}
	}
	return true
}
