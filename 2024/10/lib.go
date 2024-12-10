package main

import (
	"bufio"
	"io"
	"math"
)

const (
	start = '0'
	end   = '9'

	infinity = math.MaxInt
)

type Coord struct {
	x, y int
}

func parse(in io.Reader) [][]byte {
	var grid [][]byte
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		grid = append(grid, scanner.Bytes())
	}
	return grid
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
