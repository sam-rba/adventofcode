package main

import (
	"fmt"
	"strings"
)

type List[T any] struct {
	val  T
	next *List[T]
}

func (l *List[T]) Len() int {
	n := 0
	for p := l; p != nil; p = p.next {
		n++
	}
	return n
}

func (l *List[T]) String() string {
	var s strings.Builder
	s.WriteRune('[')
	first := true
	for p := l; p != nil; p = p.next {
		if !first {
			s.WriteString(", ")
		} else {
			first = false
		}
		s.WriteString(fmt.Sprint(p.val))
	}
	s.WriteRune(']')
	return s.String()
}
