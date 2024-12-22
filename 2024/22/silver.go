package main

import (
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"golang.org/x/sync/errgroup"
	"os"
	"runtime"
)

func main() {
	initialSecrets := make(chan int)
	go parse(os.Stdin, initialSecrets)

	secrets, total := make(chan int), make(chan int)
	go lib.Sum(secrets, total)

	var group errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	for secret := range initialSecrets {
		group.Go(func() error {
			for i := 0; i < numSecrets; i++ {
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
