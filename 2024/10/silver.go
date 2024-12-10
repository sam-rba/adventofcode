package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"github.com/sam-rba/workpool"
	"io"
	"math"
	"os"
	"slices"
)

const (
	start = '0'
	end   = '9'

	infinity = math.MaxInt
)

type Coord struct {
	x, y int
}

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

func parse(in io.Reader) [][]byte {
	var grid [][]byte
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		grid = append(grid, scanner.Bytes())
	}
	return grid
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

func neighbors(pos Coord, grid [][]byte) []Coord {
	x, y := pos.x, pos.y
	elev := grid[y][x]

	nbrs := make([]Coord, 0, 4)
	if x > 0 && grid[y][x-1] == elev+1 {
		nbrs = append(nbrs, Coord{x - 1, y})
	}
	if x < len(grid[0])-1 && grid[y][x+1] == elev+1 {
		nbrs = append(nbrs, Coord{x + 1, y})
	}
	if y > 0 && grid[y-1][x] == elev+1 {
		nbrs = append(nbrs, Coord{x, y - 1})
	}
	if y < len(grid)-1 && grid[y+1][x] == elev+1 {
		nbrs = append(nbrs, Coord{x, y + 1})
	}
	return nbrs
}
