package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"golang.org/x/sync/errgroup"
	"io"
	"log"
	"os"
	"runtime"
)

type Tile byte

const (
	empty Tile = '.'
	wall  Tile = '#'
	box   Tile = 'O'
	robot Tile = '@'
)

type Move byte

const (
	up    Move = '^'
	down  Move = 'v'
	left  Move = '<'
	right Move = '>'
)

func main() {
	r := bufio.NewReader(os.Stdin)

	grid, err := parseGrid(r)
	if err != nil {
		log.Fatal(err)
	}

	moves := make(chan Move)
	go parseMoves(r, moves)

	pos, ok := find(robot, grid)
	if !ok {
		log.Fatal("cannot find robot tile")
	}

	for move := range moves {
		pos = moveRobot(pos, move, grid)
	}

	coords, total := make(chan int), make(chan int)
	go coordsOf(boxOrigin, grid, coords)
	go lib.Sum(coords, total)

	fmt.Printf("%s: %d\n", part, <-total)
}

func parseMoves(r io.ByteReader, out chan<- Move) {
	defer close(out)
	for {
		b, err := r.ReadByte()
		if err == io.EOF {
			return
		} else if err != nil {
			log.Fatal(err)
		}
		if b != '\n' {
			out <- Move(b)
		}
	}
}

func find(tile Tile, grid [][]Tile) (lib.Point, bool) {
	for y := range grid {
		for x := range grid[y] {
			if grid[y][x] == tile {
				return lib.Point{x, y}, true
			}
		}
	}
	return lib.Point{}, false
}

func direction(m Move) lib.Vector {
	var dx, dy int
	switch m {
	case up:
		dy = -1
	case down:
		dy = 1
	case left:
		dx = -1
	case right:
		dx = 1
	default:
		panic(fmt.Sprintf("illegal move: %c", m))
	}
	return lib.Vector{dx, dy}
}

func swap(a, b lib.Point, grid [][]Tile) {
	grid[a.Y][a.X], grid[b.Y][b.X] = grid[b.Y][b.X], grid[a.Y][a.X]
}

func coordsOf(target Tile, grid [][]Tile, coords chan<- int) {
	defer close(coords)
	var group errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	for y := range grid {
		for x := range grid[y] {
			group.Go(func() error {
				if grid[y][x] == target {
					coords <- gpsCoord(x, y)
				}
				return nil
			})
		}
	}
	group.Wait()
}

func gpsCoord(x, y int) int {
	return 100*y + x
}

type Stack[T any] []T

func (s Stack[T]) push(v T) Stack[T] {
	return append(s, v)
}

func (s Stack[T]) pop() (T, Stack[T]) {
	n := len(s)
	return s[n-1], s[:n-1]
}
