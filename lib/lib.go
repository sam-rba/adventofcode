package lib

import (
	"cmp"
	"slices"
)

func Sum(in <-chan int, out chan<- int) {
	defer close(out)
	sum := 0
	for v := range in {
		sum += v
	}
	out <- sum
}

func Count[T any](in <-chan T, num chan<- int) {
	defer close(num)
	n := 0
	for range in {
		n++
	}
	num <- n
}

func Collect[T any](c <-chan T) []T {
	var s []T
	for e := range c {
		s = append(s, e)
	}
	return s
}

func Sort[T cmp.Ordered](in <-chan T, sorted chan<- T) {
	defer close(sorted)

	s := make([]T, 0)
	for e := range in {
		i, _ := slices.BinarySearch(s, e)
		s = slices.Insert(s, i, e)
	}

	for _, e := range s {
		sorted <- e
	}
}

func Min[T cmp.Ordered](in <-chan T, min chan<- T) {
	defer close(min)
	minVal := <-in
	for v := range in {
		if cmp.Less(v, minVal) {
			minVal = v
		}
	}
	min <- minVal
}

func UniqueFunc[T any](in <-chan T, out chan<- T, cmp func(T, T) int) {
	defer close(out)
	var seen []T
	for v := range in {
		if i, ok := slices.BinarySearchFunc(seen, v, cmp); !ok {
			seen = slices.Insert(seen, i, v)
		}
	}
	for _, v := range seen {
		out <- v
	}
}

func Unique[T cmp.Ordered](in <-chan T, out chan<- T) {
	UniqueFunc(in, out, cmp.Compare)
}

type Point struct {
	X, Y int
}

func (p1 Point) Add(p2 Point) Point {
	return Point{p1.X + p2.X, p1.Y + p2.Y}
}

func (p1 Point) Sub(p2 Point) Point {
	return Point{p1.X - p2.X, p1.Y - p2.Y}
}

func (p1 Point) Mul(p2 Point) Point {
	return Point{p1.X * p2.X, p1.Y * p2.Y}
}

func CmpPoint(a, b Point) int {
	if a.Y < b.Y {
		return -1
	} else if a.Y > b.Y {
		return 1
	} else if a.X < b.X {
		return -1
	} else if a.X > b.X {
		return 1
	}
	return 0
}

type Vector Point

func (v Vector) RotateCW() Vector {
	return Vector{v.Y, -v.X}
}

func (v Vector) RotateCCW() Vector {
	return Vector{-v.Y, v.X}
}

func CmpVec(v1, v2 Vector) int {
	return CmpPoint(Point(v1), Point(v2))
}

func PtAddVec(p Point, v Vector) Point {
	return p.Add(Point(v))
}
