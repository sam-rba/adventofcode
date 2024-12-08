package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/workpool"
	"io"
	"os"
	"slices"
)

type Point struct {
	x, y int
}

type Vector struct {
	x, y int
}

type Rectangle struct {
	min, max Point
}

type Antenna struct {
	pos  Point
	freq byte
}

func main() {
	nodes, bounds := parse(os.Stdin)

	antinodes, numAntinodes := make(chan Point), make(chan int)
	go countUnique(antinodes, numAntinodes, cmp)

	pool := workpool.New(workpool.DefaultSize)
	for pair := range pairs(nodes) {
		pool.Spawn(func() {
			for _, antinode := range findAntinodes(pair.a, pair.b, bounds) {
				antinodes <- antinode
			}
		})
	}
	pool.Wait()
	pool.Close()
	close(antinodes)

	fmt.Printf("%s: %d\n", part, <-numAntinodes)
}

func parse(in io.Reader) (nodes []Antenna, bounds Rectangle) {
	scanner := bufio.NewScanner(in)
	var y int
	for y = 0; scanner.Scan(); y++ {
		for x, c := range scanner.Text() {
			if x > bounds.max.x {
				bounds.max.x = x
			}

			freq := byte(c)
			if freq == '.' {
				continue
			}

			nodes = append(nodes, Antenna{
				pos:  Point{x, y},
				freq: freq,
			})
		}
	}
	bounds.max.y = y - 1

	return
}

func countUnique[T any](in <-chan T, num chan<- int, cmp func(T, T) int) {
	defer close(num)
	var seen []T
	for e := range in {
		if i, ok := slices.BinarySearchFunc(seen, e, cmp); !ok {
			seen = slices.Insert(seen, i, e)
		}
	}
	num <- len(seen)
}

func cmpPos(a, b Antenna) int { return cmp(a.pos, b.pos) }

func cmp(a, b Point) int {
	if a.y < b.y {
		return -1
	} else if a.y > b.y {
		return 1
	} else if a.x < b.x {
		return -1
	} else if a.x > b.x {
		return 1
	}
	return 0
}

type Pair[T any] struct {
	a, b T
}

func pairs[T any](s []T) <-chan Pair[T] {
	c := make(chan Pair[T])
	go func() {
		defer close(c)
		for i := range s {
			for j := i + 1; j < len(s); j++ {
				c <- Pair[T]{s[i], s[j]}
			}
		}
	}()
	return c
}

func ptInRect(p Point, r Rectangle) bool {
	return p.x >= r.min.x && p.x <= r.max.x && p.y >= r.min.y && p.y <= r.max.y
}

func vpt(p1, p2 Point) Vector {
	return Vector{p2.x - p1.x, p2.y - p1.y}
}

func (p Point) add(v Vector) Point {
	return Point{p.x + v.x, p.y + v.y}
}

func (p Point) sub(v Vector) Point {
	return Point{p.x - v.x, p.y - v.y}
}
