package main

import (
	"fmt"
	"github.com/sam-rba/workpool"
	"io"
	"log"
	"os"
	"regexp"
	"strconv"
)

func main() {
	mulInstr := regexp.MustCompile(`mul\(([0-9]{1,3}),([0-9]{1,3})\)`)
	input, err := io.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}
	matches := mulInstr.FindAllStringSubmatch(string(input), -1)

	results, total := make(chan int), make(chan int)
	go sum(results, total)
	pool := workpool.New(workpool.DefaultSize)
	for _, match := range matches {
		pool.Spawn(func() {
			a, err := strconv.Atoi(match[1])
			if err != nil {
				log.Fatalf("%s: %v", match[0], err)
			}

			b, err := strconv.Atoi(match[2])
			if err != nil {
				log.Fatalf("%s: %v", match[0], err)
			}

			results <- a * b
		})
	}
	pool.Wait()
	pool.Close()
	close(results)

	fmt.Println("silver:", <-total)
}

func sum(in <-chan int, out chan<- int) {
	defer close(out)
	sum := 0
	for v := range in {
		sum += v
	}
	out <- sum
}
