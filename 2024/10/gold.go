package main

import (
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"github.com/sam-rba/workpool"
	"os"
)

func main() {
	grid := parse(os.Stdin)

	ratings, total := make(chan int), make(chan int)
	go lib.Sum(ratings, total)

	pool := workpool.New(workpool.DefaultSize)
	defer pool.Close()
	starts, ends := endpoints(grid)
	for i := range starts {
		for j := range ends {
			pool.Spawn(func() {
				ratings <- len(allPaths(starts[i], ends[j], grid))
			})
		}
	}
	pool.Wait()
	close(ratings)
	fmt.Println("gold:", <-total)
}

func endpoints(grid [][]byte) (starts, ends []Coord) {
	for y := range grid {
		for x := range grid[y] {
			if grid[y][x] == start {
				starts = append(starts, Coord{x, y})
			} else if grid[y][x] == end {
				ends = append(ends, Coord{x, y})
			}
		}
	}
	return
}

func allPaths(start, end Coord, grid [][]byte) [][]Coord {
	visited := make([][]bool, len(grid))
	for i := range visited {
		visited[i] = make([]bool, len(grid[0]))
	}
	path, paths := make([]Coord, 0), make([][]Coord, 0)
	dfs(start, end, grid, visited, &path, &paths)
	return paths
}

func dfs(pos, end Coord, grid [][]byte, visited [][]bool, path *[]Coord, paths *[][]Coord) {
	if visited[pos.y][pos.x] {
		return
	}

	visited[pos.y][pos.x] = true
	*path = append(*path, pos)

	if pos == end {
		clone := make([]Coord, len(*path))
		if copy(clone, *path) != len(*path) {
			panic("copy failed")
		}
		*paths = append(*paths, clone)
		*path = (*path)[:len(*path)-1]
		visited[pos.y][pos.x] = false
		return
	}

	adjs := neighbors(pos, grid)
	for _, adj := range adjs {
		dfs(adj, end, grid, visited, path, paths)
	}

	*path = (*path)[:len(*path)-1]
	visited[pos.y][pos.x] = false
}
