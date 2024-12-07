package main

import (
	"fmt"
	"os"
	"slices"
)

func main() {
	obstacles, guard := parse(os.Stdin)
	bounds := bounds(obstacles)

	n := 0
	for y := bounds.min.y; y <= bounds.max.y; y++ {
		for x := bounds.min.x; x <= bounds.max.x; x++ {
			newObst := Point{x, y} // New obstacle.

			i, ok := slices.BinarySearchFunc(obstacles, newObst, cmp)
			if ok {
				continue // Already an obstacle here.
			}

			obstacles = slices.Insert(obstacles, i, newObst)
			if isLoop(guard, obstacles, bounds) {
				n++
			}
			obstacles = slices.Delete(obstacles, i, i+1)
		}
	}

	fmt.Println("gold:", n)
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
