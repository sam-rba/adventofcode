package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"slices"
)

type Point struct {
	x, y int
}

type Direction struct {
	dx, dy int
}

type Guard struct {
	pos Point
	dir Direction
}

type Rectangle struct {
	min, max Point
}

func main() {
	obstacles, guard := parse(os.Stdin)
	bounds := bounds(obstacles)
	var visited []Point
	for ptInRect(guard.pos, bounds) {
		visited = visit(guard, visited)
		guard = move(guard, obstacles)
	}
	fmt.Println("silver:", len(visited))
}

func parse(in io.Reader) (obstacles []Point, guard Guard) {
	guard.dir.dy = -1
	scanner := bufio.NewScanner(in)
	for y := 0; scanner.Scan(); y++ {
		line := scanner.Text()
		for x := range line {
			switch line[x] {
			case '.':
				continue
			case '#':
				p := Point{x, y}
				i, _ := slices.BinarySearchFunc(obstacles, p, cmp)
				obstacles = slices.Insert(obstacles, i, p)
			case '^':
				guard.pos = Point{x, y}
			default:
				log.Fatal("bad input:", line)
			}
		}
	}
	return
}

func bounds(obstacles []Point) Rectangle {
	var r Rectangle
	for _, p := range obstacles {
		if p.x > r.max.x {
			r.max.x = p.x
		}
		if p.y > r.max.y {
			r.max.y = p.y
		}
	}
	return r
}

func ptInRect(p Point, r Rectangle) bool {
	return p.x >= r.min.x && p.x <= r.max.x && p.y >= r.min.y && p.y <= r.max.y
}

func visit(guard Guard, visited []Point) []Point {
	i, ok := slices.BinarySearchFunc(visited, guard.pos, cmp)
	if !ok {
		return slices.Insert(visited, i, guard.pos)
	}
	return visited
}

func move(guard Guard, obstacles []Point) Guard {
	newpos := Point{
		guard.pos.x + guard.dir.dx,
		guard.pos.y + guard.dir.dy,
	}

	_, hit := slices.BinarySearchFunc(obstacles, newpos, cmp)
	if hit {
		guard.dir = rotate(guard.dir)
	} else {
		guard.pos = newpos
	}

	return guard
}

func rotate(d Direction) Direction {
	return Direction{-d.dy, d.dx}
}

func cmp(p1, p2 Point) int {
	if p1.x < p2.x {
		return -1
	} else if p1.x > p2.x {
		return 1
	} else if p1.y < p2.y {
		return -1
	} else if p1.y > p2.y {
		return 1
	}
	return 0
}
