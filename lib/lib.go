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
