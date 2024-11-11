package main

import (
	"fmt"
	"log"
	"os"
)

const (
	diskSize          = 70_000_000
	requiredFreeSpace = 30_000_000
)

func main() {
	root, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	used := root.Size()
	free := diskSize - used
	required := requiredFreeSpace - free
	fmt.Printf("used: %d\nfree: %d\nrequired: %d\n", used, free, required)
	size, ok := smallestAtLeast(root, required)
	if !ok {
		log.Fatal("no directory with size at least", required)
	}
	fmt.Println("gold:", size)
}

func smallestAtLeast(dot *Dir, thresh int) (int, bool) {
	dotSize := dot.Size()
	if dotSize < thresh {
		return 0, false
	}

	smallest := dotSize
	for name, entry := range dot.entries {
		if name == "." || name == ".." {
			continue
		}
		dir, ok := entry.(*Dir)
		if !ok {
			continue
		}
		if size, ok := smallestAtLeast(dir, thresh); ok {
			if size < smallest {
				smallest = size
			}
		}
	}
	return smallest, true
}
