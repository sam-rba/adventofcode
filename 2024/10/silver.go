package main

import (
	"container/heap"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"github.com/sam-rba/workpool"
	"os"
	"slices"
)

func main() {
	grid := parse(os.Stdin)

	scores, total := make(chan int), make(chan int)
	go lib.Sum(scores, total)

	pool := workpool.New(workpool.DefaultSize)
	defer pool.Close()
	for y := range grid {
		for x := range grid[y] {
			if grid[y][x] != start {
				continue
			}
			pool.Spawn(func() {
				scores <- score(Coord{x, y}, grid)
			})
		}
	}
	pool.Wait()
	close(scores)
	fmt.Println("silver:", <-total)
}

func score(trailhead Coord, grid [][]byte) int {
	dists := dijkstra(trailhead, grid)
	score := 0
	for node, dist := range dists {
		if grid[node.y][node.x] == end && dist != infinity {
			score++
		}
	}
	return score
}

func dijkstra(start Coord, grid [][]byte) (dist map[Coord]int) {
	q, dists := build(start, grid)
	for q.Len() > 0 {
		tile := heap.Pop(&q).(*Tile)
		adjs := neighbors(tile.pos, grid)
		for _, adj := range adjs {
			i := slices.IndexFunc(q, func(tile *Tile) bool {
				return tile.pos == adj
			})
			if i < 0 {
				continue // already visited.
			}
			neighbor := q[i]
			alt := tile.distance + 1
			if alt > 0 && alt < neighbor.distance {
				dists[neighbor.pos] = alt
				q.update(neighbor, alt)
			}
		}
	}
	return dists
}

func build(start Coord, grid [][]byte) (q PriorityQueue, dists map[Coord]int) {
	q = make(PriorityQueue, len(grid)*len(grid[0]))
	dists = make(map[Coord]int)
	i := 0
	for y := range grid {
		for x := range grid[y] {
			pos := Coord{x, y}
			var dist int
			if pos == start {
				dist = 0
			} else {
				dist = infinity
			}
			q[i] = &Tile{pos, dist, i}
			dists[pos] = dist
			i++
		}
	}
	heap.Init(&q)
	return q, dists
}
