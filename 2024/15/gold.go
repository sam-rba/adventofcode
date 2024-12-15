package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
)

const (
	boxLeft  Tile = '['
	boxRight Tile = ']'

	boxOrigin = boxLeft

	part = "gold"
)

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

		if grid != nil && 2*len(line) != len(grid[0]) {
			return nil, fmt.Errorf("mismatched line lengths in grid:\n%s\n%s", grid[0], line)
		}

		row := make([]Tile, 2*len(line))
		for i := range line {
			tile := Tile(line[i])
			switch tile {
			case empty, wall:
				row[2*i], row[2*i+1] = tile, tile
			case box:
				row[2*i], row[2*i+1] = boxLeft, boxRight
			case robot:
				row[2*i], row[2*i+1] = robot, empty
			default:
				return nil, fmt.Errorf("illegal tile: %c", line[i])
			}
		}
		grid = append(grid, row)
	}
	return grid, nil
}

func moveRobot(robotPos lib.Point, move Move, grid [][]Tile) lib.Point {
	v := direction(move)
	newP := lib.PtAddVec(robotPos, v)

	stack, ok := pushBoxes(newP, move, grid)
	if !ok {
		return robotPos
	}

	for len(stack) > 0 {
		var box lib.Point
		box, stack = stack.pop()
		newP := lib.PtAddVec(box, v)
		moveBox(box, newP, grid)
	}

	swap(robotPos, newP, grid)
	return newP
}

func pushBoxes(p lib.Point, move Move, grid [][]Tile) (Stack[lib.Point], bool) {
	var stack Stack[lib.Point]
	queue := Queue[lib.Point]{p}

	for len(queue) > 0 {
		p, queue = queue.pop()

		switch grid[p.Y][p.X] {
		case wall:
			return nil, false

		case boxLeft:
			stack = stack.push(p)
			switch move {
			case up:
				queue = queue.push(above(p))
				queue = queue.push(above(rightOf(p)))
			case down:
				queue = queue.push(below(p))
				queue = queue.push(below(rightOf(p)))
			case right:
				queue = queue.push(rightOf(rightOf(p)))
			default:
				panic("illegal direction")
			}

		case boxRight:
			stack = stack.push(leftOf(p))
			switch move {
			case up:
				queue = queue.push(above(p))
				queue = queue.push(above(leftOf(p)))
			case down:
				queue = queue.push(below(p))
				queue = queue.push(below(leftOf(p)))
			case left:
				queue = queue.push(leftOf(leftOf(p)))
			default:
				panic("illegal direction")
			}
		}
	}

	return stack, true
}

func moveBox(pos, newPos lib.Point, grid [][]Tile) {
	right := rightOf(pos)
	grid[pos.Y][pos.X], grid[right.Y][right.X] = empty, empty

	right = rightOf(newPos)
	grid[newPos.Y][newPos.X], grid[right.Y][right.X] = boxLeft, boxRight
}

type Queue[T any] []T

func (q Queue[T]) push(v T) Queue[T] {
	return append(q, v)
}

func (q Queue[T]) pop() (T, Queue[T]) {
	return q[0], q[1:]
}

func above(p lib.Point) lib.Point {
	return lib.Point{p.X, p.Y - 1}
}

func below(p lib.Point) lib.Point {
	return lib.Point{p.X, p.Y + 1}
}

func leftOf(p lib.Point) lib.Point {
	return lib.Point{p.X - 1, p.Y}
}

func rightOf(p lib.Point) lib.Point {
	return lib.Point{p.X + 1, p.Y}
}
