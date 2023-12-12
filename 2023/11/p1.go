package main

import (
	"bufio"
	"fmt"
	"os"
)

const GALAXY = '#'

type CoordMap map[uint]map[uint]bool

type Coords struct {
	x uint
	y uint
}

type CoordPair struct {
	a Coords
	b Coords
}

func main() {
	var (
		galaxies             CoordMap
		pairs                []CoordPair
		pair                 CoordPair
		emptyRows, emptyCols map[uint]bool
		dist                 uint
	)

	galaxies = readInput()
	pairs = toPairs(galaxies)
	emptyRows, emptyCols = empty(galaxies)
	dist = 0
	for _, pair = range pairs {
		dist += distance(pair.a, pair.b, emptyRows, emptyCols)
	}
	fmt.Printf("part 1: %d\n", dist)
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

func toPairs(galaxies CoordMap) (pairs []CoordPair) {
	var (
		coords  []Coords
		i, j, p int
	)

	coords = toArr(galaxies)
	pairs = make([]CoordPair, chooseTwo(len(coords)))

	p = 0
	for i = len(coords) - 1; i >= 0; i-- {
		for j = i - 1; j >= 0; j-- {
			pairs[p] = CoordPair{coords[i], coords[j]}
			p++
		}
	}
	return pairs
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

func distance(a, b Coords, emptyRows, emptyCols map[uint]bool) uint {
	var (
		x, y uint
	)

	if a.x > b.x {
		x = a.x - b.x + countBetween(emptyCols, b.x, a.x)
	} else if b.x >= a.x {
		x = b.x - a.x + countBetween(emptyCols, a.x, b.x)
	}

	if a.y > b.y {
		y = a.y - b.y + countBetween(emptyRows, b.y, a.y)
	} else if b.y >= a.y {
		y = b.y - a.y + countBetween(emptyRows, a.y, b.y)
	}

	return x + y
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

func toArr(galaxies CoordMap) []Coords {
	var (
		coords []Coords
		x, y   uint
		row    map[uint]bool
	)

	for y, row = range galaxies {
		for x = range row {
			coords = append(coords, Coords{x, y})
		}
	}
	return coords
}

// C(n, 2)
func chooseTwo(n int) int {
	return (n*n - n) / 2
}
