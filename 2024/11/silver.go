package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
)

const numBlinks = 25

func main() {
	stones, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	for blink := 0; blink < numBlinks; blink++ {
		for stone := stones; stone != nil; stone = stone.next {
			if stone.val == 0 {
				stone.val = 1
			} else if s := fmt.Sprint(stone.val); len(s)%2 == 0 {
				split(stone, s)
				stone = stone.next
			} else {
				stone.val *= 2024
			}
		}
	}

	fmt.Println("silver:", stones.Len())
}

func parse(in io.Reader) (*List[int], error) {
	var stones, dot *List[int]
	scanner := bufio.NewScanner(in)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		n, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return &List[int]{}, err
		}
		if dot == nil {
			dot = &List[int]{n, nil}
			stones = dot
		} else {
			dot.next = &List[int]{n, nil}
			dot = dot.next
		}
	}
	return stones, nil
}

func split(stone *List[int], val string) {
	left, right := val[:len(val)/2], val[len(val)/2:]

	n, err := strconv.Atoi(left)
	if err != nil {
		log.Fatal(err)
	}
	stone.val = n

	n, err = strconv.Atoi(right)
	if err != nil {
		log.Fatal(err)
	}
	stone.next = &List[int]{n, stone.next}
}
