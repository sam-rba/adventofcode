package main

import (
	"fmt"
	"os"
)

func main() {
	obstacles, guard := parse(os.Stdin)
	bounds := bounds(obstacles)
	var visited []Point
	for ptInRect(guard.pos, bounds) {
		visited = visit(guard, visited)
		guard = move(guard, obstacles)
	}
	fmt.Println("silver:", len(visited))
}
