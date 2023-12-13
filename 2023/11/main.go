package main

import (
	"bufio"
	"fmt"
	"os"
)

const (
	GALAXY = '#'
	// growth factors for each part
	FACTOR1 = 2
	FACTOR2 = 1_000_000
)

type CoordMap map[uint]map[uint]bool

type Coord struct {
	x uint
	y uint
}

func main() {
	var (
		emptyRows, emptyCols map[uint]bool
		galaxyMap            CoordMap
		galaxyArr            []Coord
		dist1, dist2         uint
		i, j                 int
	)

	galaxyMap = readInput()
	emptyRows, emptyCols = empty(galaxyMap)
	galaxyArr = toArr(galaxyMap)

	dist1, dist2 = 0, 0
	for i = len(galaxyArr) - 1; i >= 0; i-- {
		for j = i - 1; j >= 0; j-- {
			dist1 += distance(galaxyArr[i], galaxyArr[j], emptyRows, emptyCols, FACTOR1)
			dist2 += distance(galaxyArr[i], galaxyArr[j], emptyRows, emptyCols, FACTOR2)
		}
	}
	fmt.Printf("part 1: %d\npart2: %d\n", dist1, dist2)
}

func readInput() (galaxies CoordMap) {
	var (
		stdio *bufio.Scanner
		line  string
		row   uint
		col   int
		c     rune
		ok    bool
	)

	stdio = bufio.NewScanner(os.Stdin)
	galaxies = make(map[uint]map[uint]bool)
	for row = 0; stdio.Scan(); row++ {
		line = stdio.Text()
		for col, c = range line {
			if c == GALAXY {
				if _, ok = galaxies[row]; !ok {
					galaxies[row] = make(map[uint]bool)
				}
				galaxies[row][uint(col)] = true
			}
		}
	}
	return galaxies
}

func empty(galaxies CoordMap) (rows map[uint]bool, cols map[uint]bool) {
	var (
		i  uint
		ok bool
	)

	rows = make(map[uint]bool)
	for i = max(galaxies); i > 0; i-- {
		if _, ok = galaxies[i-1]; !ok {
			rows[i-1] = true
		}
	}

	galaxies = transpose(galaxies)
	cols = make(map[uint]bool)
	for i = max(galaxies); i > 0; i-- {
		if _, ok = galaxies[i-1]; !ok {
			cols[i-1] = true
		}
	}
	return rows, cols
}

func distance(a, b Coord, emptyRows, emptyCols map[uint]bool, factor uint) uint {
	var (
		x, y uint
	)

	if a.x > b.x {
		x = a.x - b.x + ((factor - 1) * countBetween(emptyCols, b.x, a.x))
	} else if b.x >= a.x {
		x = b.x - a.x + ((factor - 1) * countBetween(emptyCols, a.x, b.x))
	}

	if a.y > b.y {
		y = a.y - b.y + ((factor - 1) * countBetween(emptyRows, b.y, a.y))
	} else if b.y >= a.y {
		y = b.y - a.y + ((factor - 1) * countBetween(emptyRows, a.y, b.y))
	}

	return x + y
}

func max[T any](set map[uint]T) uint {
	var max, n uint

	max = 0
	for n = range set {
		if n > max {
			max = n
		}
	}
	return max
}

func countBetween[T any](set map[uint]T, lo, hi uint) uint {
	var n, i uint

	n = 0
	for i = range set {
		if i > lo && i < hi {
			n++
		}
	}
	return n
}

func transpose(galaxies CoordMap) CoordMap {
	var (
		transposed CoordMap
		x, y       uint
		row        map[uint]bool
		ok         bool
	)

	transposed = make(CoordMap)
	for y, row = range galaxies {
		for x = range row {
			if _, ok = transposed[x]; !ok {
				transposed[x] = make(map[uint]bool)
			}
			transposed[x][y] = true
		}
	}
	return transposed
}

func toArr(galaxies CoordMap) []Coord {
	var (
		coords []Coord
		x, y   uint
		row    map[uint]bool
	)

	for y, row = range galaxies {
		for x = range row {
			coords = append(coords, Coord{x, y})
		}
	}
	return coords
}
