package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"golang.org/x/sync/errgroup"
	"io"
	"log"
	"os"
	"runtime"
	"strconv"
)

const (
	numSecret = 2000
	pruneDiv  = 16777216
)

func next(secret int) int {
	secret = prune(mix(secret, secret*64))
	secret = prune(mix(secret, secret/32))
	secret = prune(mix(secret, secret*2048))
	return secret
}

func mix(a, b int) int {
	return a ^ b
}

func prune(a int) int {
	return a % pruneDiv
}

func main() {
	initialSecrets := make(chan int)
	go parse(os.Stdin, initialSecrets)

	secrets, total := make(chan int), make(chan int)
	go lib.Sum(secrets, total)

	var group errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	for secret := range initialSecrets {
		group.Go(func() error {
			for i := 0; i < numSecret; i++ {
				secret = next(secret)
			}
			secrets <- secret
			return nil
		})
	}
	group.Wait()
	close(secrets)

	fmt.Println("silver:", <-total)
}

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
