package main

import (
	"fmt"
	"log"
	"os"
)

const thresholdSize = 100_000

type DirEntry interface {
	IsDir() bool
	Name() string
	Size() int
}

func main() {
	fs, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("/ size:", fs.Size())

	fmt.Println("silver:", dirsAtMostSize(fs, thresholdSize))
}

func dirsAtMostSize(dot *Dir, maxSize int) int {
	sum := 0
	size := dot.Size()
	if size <= maxSize {
		sum += size
	}
	for name, entry := range dot.entries {
		if name == "." || name == ".." {
			continue
		}
		if subdir, ok := entry.(*Dir); ok {
			sum += dirsAtMostSize(subdir, maxSize)
		}
	}
	return sum
}
