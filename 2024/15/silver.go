package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"slices"
)

const (
	boxOrigin = box

	part = "silver"
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
