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
	"slices"
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
	go boxCoords(grid, coords)
	go lib.Sum(coords, total)

	fmt.Println("silver:", <-total)
}

func parseGrid(r *bufio.Reader) ([][]Tile, error) {
	var grid [][]Tile
	for {
		line, err := r.ReadBytes('\n')
		if err != nil {
			return nil, err
		}
		if len(line) <= 1 {
			break
		}
		line = line[:len(line)-1] // strip \n.

		if grid != nil && len(line) != len(grid[0]) {
			return nil, fmt.Errorf("mismatched line lengths in grid:\n%s\n%s", grid[0], line)
		}

		grid = append(grid, make([]Tile, len(line)))
		for i := range line {
			grid[len(grid)-1][i] = Tile(line[i])
		}
	}
	return grid, nil
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

func moveRobot(robotPos lib.Point, move Move, grid [][]Tile) lib.Point {
	stack := pushUntil(robotPos, move, grid, wall, empty)

	var oldP, newP lib.Point
	oldP, stack = stack.pop()

	for len(stack) > 0 {
		newP = oldP
		oldP, stack = stack.pop()

		if grid[newP.Y][newP.X] != empty {
			if len(stack) > 0 {
				return stack[0]
			}
			return oldP
		}

		swap(oldP, newP, grid)
	}

	return newP
}

func pushUntil(start lib.Point, move Move, grid [][]Tile, ends ...Tile) Stack[lib.Point] {
	stack := make(Stack[lib.Point], 0)
	p := start
	v := direction(move)
	for !slices.Contains(ends, grid[p.Y][p.X]) {
		stack = stack.push(p)
		p = lib.PtAddVec(p, v)
	}
	return stack.push(p)
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

func boxCoords(grid [][]Tile, coords chan<- int) {
	defer close(coords)
	var group errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	for y := range grid {
		for x := range grid[y] {
			group.Go(func() error {
				if grid[y][x] == box {
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
