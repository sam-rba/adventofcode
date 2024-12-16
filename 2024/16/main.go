package main

import (
	"bufio"
	"fmt"
	"github.com/ethereum/go-ethereum/common/prque"
	"github.com/sam-rba/adventofcode/lib"
	"io"
	"log"
	"math"
	"os"
)

const (
	infinity = math.MaxInt

	forwardCost = 1
	rotateCost  = 1000

	wall      = '#'
	startTile = 'S'
	endTile   = 'E'
)

var (
	north = lib.Vector{0, -1}
	east  = lib.Vector{1, 0}
	south = lib.Vector{0, 1}
	west  = lib.Vector{-1, 0}

	startDir = east
)

type Node struct {
	pos lib.Point  // position.
	dir lib.Vector // direction.
}

func main() {
	grid, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	startPos, ok := find(startTile, grid)
	if !ok {
		log.Fatal("can't find start tile")
	}
	start := Node{startPos, startDir}

	dists, paths := dijkstra(start, grid)

	endPos, ok := find(endTile, grid)
	if !ok {
		log.Fatal("can't find end tile")
	}

	print(grid, dists, paths, endPos)

	fmt.Println("silver:", minDist(endPos, dists))
	fmt.Println("gold:", countPathNodes(endPos, dists, paths))
}

func parse(in io.Reader) ([][]byte, error) {
	var grid [][]byte
	r := bufio.NewReader(in)
	for {
		line, err := r.ReadBytes('\n')
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, err
		}
		grid = append(grid, line[:len(line)-1])
	}
	return grid, nil
}

func find(target byte, grid [][]byte) (lib.Point, bool) {
	for y := range grid {
		for x := range grid[y] {
			if grid[y][x] == target {
				return lib.Point{x, y}, true
			}
		}
	}
	return lib.Point{}, false
}

func dijkstra(start Node, grid [][]byte) (dists map[Node]int, paths map[Node][]Node) {
	dists = build(grid)
	dists[start] = 0
	paths = make(map[Node][]Node)

	q := prque.New[int, Node](func(data Node, index int) {})
	q.Push(start, 0)

	for q.Size() > 0 {
		n, _ := q.Pop()
		adjs := neighbors(n, grid)
		for adj, weight := range adjs {
			alt := dists[n] + weight
			if alt < dists[adj] {
				dists[adj] = alt
				paths[adj] = []Node{n}
				q.Push(adj, alt)
			} else if alt == dists[adj] {
				paths[adj] = append(paths[adj], n)
			}
		}
	}

	return dists, paths
}

func minDist(pos lib.Point, dists map[Node]int) int {
	min := infinity
	for _, n := range allOrientations(pos) {
		if dist, ok := dists[n]; ok && dist < min {
			min = dist
		}
	}
	return min
}

func countPathNodes(end lib.Point, dists map[Node]int, paths map[Node][]Node) int {
	pathNodes, unique, num := make(chan Node), make(chan Node), make(chan int)
	go lib.UniqueFunc(pathNodes, unique, cmpNodePos)
	go lib.Count(unique, num)

	traverseShortestPaths(end, dists, paths, func(n Node) {
		pathNodes <- n
	})
	close(pathNodes)
	return <-num
}

func print(grid [][]byte, dists map[Node]int, paths map[Node][]Node, end lib.Point) {
	traverseShortestPaths(end, dists, paths, func(n Node) {
		grid[n.pos.Y][n.pos.X] = 'O'
	})

	for y := range grid {
		fmt.Println(string(grid[y]))
	}
}

func traverseShortestPaths(end lib.Point, dists map[Node]int, paths map[Node][]Node, f func(n Node)) {
	minPathLen := minDist(end, dists)
	for _, n := range allOrientations(end) {
		if dists[n] == minPathLen {
			traverse(n, paths, f)
		}
	}
}

func traverse(end Node, paths map[Node][]Node, f func(n Node)) {
	f(end)
	if prev, ok := paths[end]; ok {
		for _, n := range prev {
			traverse(n, paths, f)
		}
	}
}

func build(grid [][]byte) (dists map[Node]int) {
	dists = make(map[Node]int)
	for y := range grid {
		for x := range grid[y] {
			p := lib.Point{x, y}
			nodes := allOrientations(p)
			for _, n := range nodes {
				dists[n] = infinity
			}
		}
	}
	return dists
}

// Returns map of neighbor->weight
func neighbors(n Node, grid [][]byte) map[Node]int {
	left := Node{n.pos, n.dir.RotateCCW()}
	right := Node{n.pos, n.dir.RotateCW()}
	front := Node{lib.PtAddVec(n.pos, n.dir), n.dir}

	nbrs := make(map[Node]int)
	nbrs[left] = rotateCost
	nbrs[right] = rotateCost
	if front.pos.Y < len(grid) && front.pos.X < len(grid[0]) && grid[front.pos.Y][front.pos.X] != wall {
		nbrs[front] = forwardCost
	}
	return nbrs
}

func allOrientations(p lib.Point) []Node {
	return []Node{
		{p, north},
		{p, east},
		{p, south},
		{p, west},
	}
}

func cmpNode(n1, n2 Node) int {
	if ord := cmpNodePos(n1, n2); ord != 0 {
		return ord
	}
	return lib.CmpVec(n1.dir, n2.dir)
}

func cmpNodePos(n1, n2 Node) int {
	return lib.CmpPoint(n1.pos, n2.pos)
}
