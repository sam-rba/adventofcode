package main

import "fmt"

func priority(item rune) int {
	if item >= 'a' && item <= 'z' {
		return int(item - 'a' + 1)
	} else if item >= 'A' && item <= 'Z' {
		return int(item - 'A' + 1 + ('z' - 'a' + 1))
	}
	panic(fmt.Sprint("invalid item:", item))
}
