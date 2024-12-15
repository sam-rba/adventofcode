package main

import (
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"golang.org/x/sync/errgroup"
	"math"
	"os"
	"runtime"
)

const (
	infinity = math.MaxInt

	costA  = 3
	costB  = 1
	cutoff = 100
)

type Machine struct {
	da, db lib.Vector
	prize  lib.Point
}

func main() {
	machines := make(chan Machine)
	go parse(os.Stdin, machines)

	costs, total := make(chan int), make(chan int)
	go lib.Sum(costs, total)

	var group errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	for machine := range machines {
		group.Go(func() error {
			if c, ok := machine.cost(); ok {
				costs <- c
			}
			return nil
		})
	}
	group.Wait()
	close(costs)

	fmt.Println("silver:", <-total)
}

func (m Machine) cost() (int, bool) {
	costs, minCost := make(chan int), make(chan int)
	go lib.Min(costs, minCost)
	costs <- infinity

	var group errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	maxA := min(m.prize.X/m.da.X, m.prize.Y/m.da.Y, cutoff)
	for a := 0; a <= maxA; a++ {
		group.Go(func() error {
			dx := m.prize.X - a*m.da.X
			dy := m.prize.Y - a*m.da.Y
			if dx%m.db.X != 0 || dy%m.db.Y != 0 {
				return nil
			}
			bx := dx / m.db.X
			by := dy / m.db.Y
			if bx != by {
				return nil
			}
			b := bx // == by

			costs <- a*costA + b*costB

			return nil
		})
	}
	group.Wait()
	close(costs)

	if c := <-minCost; c != infinity {
		return c, true
	}
	return -1, false
}
