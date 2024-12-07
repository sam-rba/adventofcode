package main

import (
	"fmt"
	"github.com/sam-rba/workpool"
	"os"
	"slices"
)

func main() {
	obstacles, guard := parse(os.Stdin)
	bounds := bounds(obstacles)
	start := guard.pos

	loops, numLoops := make(chan int), make(chan int)
	go count(loops, numLoops)

	pool := workpool.New(workpool.DefaultSize)
	for y := bounds.min.y; y <= bounds.max.y; y++ {
		for x := bounds.min.x; x <= bounds.max.x; x++ {
			newObst := Point{x, y} // New obstacle.

			if newObst.x == start.x && newObst.y == start.y {
				continue // Cannot place at start position.
			}

			i, ok := slices.BinarySearchFunc(obstacles, newObst, cmp)
			if ok {
				continue // Already an obstacle here.
			}

			pool.Spawn(func() {
				newObstacles := make([]Point, len(obstacles), len(obstacles)+1)
				if copy(newObstacles, obstacles) != len(obstacles) {
					panic("copy failed")
				}
				newObstacles = slices.Insert(newObstacles, i, newObst)
				if isLoop(guard, newObstacles, bounds) {
					loops <- 1
				}
			})
		}
	}
	pool.Wait()
	pool.Close()
	close(loops)

	fmt.Println("gold:", <-numLoops)
}

func isLoop(guard Guard, obstacles []Point, bounds Rectangle) bool {
	var visitedPts []Point
	var visitedDirs []Direction

	for ptInRect(guard.pos, bounds) {
		i, visited := slices.BinarySearchFunc(visitedPts, guard.pos, cmp)

		if visited {
			dir := visitedDirs[i]
			if dir.dx == guard.dir.dx && dir.dy == guard.dir.dy {
				return true
			}
		} else {
			visitedPts = slices.Insert(visitedPts, i, guard.pos)
			visitedDirs = slices.Insert(visitedDirs, i, guard.dir)
		}

		guard = move(guard, obstacles)
	}

	return false
}

func count[T any](c <-chan T, num chan<- int) {
	n := 0
	for range c {
		n++
	}
	num <- n
	close(num)
}
