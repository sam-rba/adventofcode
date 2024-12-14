package main

import (
	"flag"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"golang.org/x/sync/errgroup"
	"os"
	"runtime"
)

type Quadrant int

const (
	QNone Quadrant = iota
	Q1
	Q2
	Q3
	Q4
)

func main() {
	flag.Parse()

	robots := make(chan Robot)
	go parse(os.Stdin, robots)

	quadrants, robotsPerQuadrant := make(chan Quadrant), make(chan int)
	go countQuadrants(quadrants, robotsPerQuadrant)
	safety := make(chan int)
	go mul(robotsPerQuadrant, safety)

	var group errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	for robot := range robots {
		group.Go(func() error {
			robot.move(steps)
			quadrants <- quadrant(robot.p)
			return nil
		})
	}
	group.Wait()
	close(quadrants)

	fmt.Println("silver:", <-safety)
}

func countQuadrants(quadrants <-chan Quadrant, numEach chan<- int) {
	defer close(numEach)
	var count [Q4]int
	for q := range quadrants {
		if q != QNone {
			count[q-1]++
		}
	}
	for i := range count {
		numEach <- count[i]
	}
}

func mul(factors <-chan int, product chan<- int) {
	defer close(product)
	prod := 1
	for f := range factors {
		prod *= f
	}
	product <- prod
}

func quadrant(p lib.Point) Quadrant {
	mid := lib.Point{*width / 2, *height / 2}

	if p.Y < mid.Y {
		if p.X > mid.X {
			return Q1
		} else if p.X < mid.X {
			return Q2
		}
	} else if p.Y > mid.Y {
		if p.X < mid.X {
			return Q3
		} else if p.X > mid.X {
			return Q4
		}
	}

	return QNone
}
