package main

import (
	"flag"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"golang.org/x/sync/errgroup"
	"log"
	"os"
	"runtime"
	"runtime/pprof"
	"strings"
)

type token struct{}

var cpuprofile = flag.String("cpuprofile", "", "write cpu profile to `file`")
var memprofile = flag.String("memprofile", "", "write memory profile to `file`")

func main() {
	flag.Parse()

	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal("could not create CPU profile: ", err)
		}
		defer f.Close() // error handling omitted for example
		if err := pprof.StartCPUProfile(f); err != nil {
			log.Fatal("could not start CPU profile: ", err)
		}
		defer pprof.StopCPUProfile()
	}

	towels, designs, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	possible, numPossible := make(chan token), make(chan int)
	go lib.Count(possible, numPossible)

	var group errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	memo := NewMap[string, bool]()
	for design := range designs {
		group.Go(func() error {
			if isPossible(design, towels, memo) {
				possible <- token{}
			}
			return nil
		})
	}
	group.Wait()
	close(possible)

	fmt.Println("silver:", <-numPossible)

	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Fatal("could not create memory profile: ", err)
		}
		defer f.Close() // error handling omitted for example
		runtime.GC()    // get up-to-date statistics
		if err := pprof.WriteHeapProfile(f); err != nil {
			log.Fatal("could not write memory profile: ", err)
		}
	}
}

func isPossible(design string, towels []string, memo Map[string, bool]) bool {
	if len(design) == 0 {
		return true
	}
	if possible, ok := memo.Get(design); ok {
		return possible
	}

	for _, towel := range towels {
		if strings.HasPrefix(design, towel) {
			if isPossible(design[len(towel):], towels, memo) {
				memo.Set(design, true)
				return true
			}
		}
	}
	memo.Set(design, false)
	return false
}
