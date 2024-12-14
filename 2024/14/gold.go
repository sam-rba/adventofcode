package main

import (
	"flag"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"golang.org/x/sync/errgroup"
	"image"
	"image/color"
	"image/jpeg"
	"log"
	"math"
	"os"
	"slices"
)

const (
	cutoff = 10_000

	infinity = math.MaxInt
)

type token struct{}

func main() {
	flag.Parse()

	robots := make(chan Robot)
	go parse(os.Stdin, robots)

	posChan, kill := make(chan lib.Point), make(chan token)

	numRobots := 0
	for robot := range robots {
		numRobots++
		go positions(robot, posChan, kill)
	}
	defer func() {
		for ; numRobots > 0; numRobots-- {
			kill <- token{}
		}
		close(kill)
		close(posChan)
	}()

	if err := os.MkdirAll("imgs", 0755); err != nil {
		log.Fatal(err)
	}

	posns := []lib.Point{{0, 0}, {0, 0}, {2, 2}}
	writeImg(posns, "foo.jpg")

	longest := 0
	var longestT int
	var longestPosns []lib.Point
	for t := 0; t < cutoff; t++ {
		posns := collectNSorted(posChan, numRobots, lib.CmpPoint)
		if err := writeImg(posns, fmt.Sprintf("imgs/%06d.jpg", t)); err != nil {
			log.Fatal(err)
		}
		n := longestRow(posns)
		if n > longest {
			longest = n
			longestT = t
			longestPosns = posns
		}
	}
	fmt.Println("gold:", longestT)
	if err := writeImg(longestPosns, "tree.jpg"); err != nil {
		log.Fatal(err)
	}
}

func positions(r Robot, position chan<- lib.Point, kill <-chan token) {
	for {
		select {
		case position <- r.p:
			r.move(1)
		case <-kill:
			return
		}
	}
}

func collectNSorted[T any](in <-chan T, n int, cmp func(T, T) int) []T {
	s := make([]T, 0, n)
	for ; n > 0; n-- {
		v := <-in
		i, _ := slices.BinarySearchFunc(s, v, cmp)
		s = slices.Insert(s, i, v)
	}
	return s
}

func longestRow(posns []lib.Point) int {
	lens, longest := make(chan int), make(chan int)
	go max(lens, longest)

	var group errgroup.Group
	for y := 0; y < *height; y++ {
		group.Go(func() error {
			lens <- numContiguous(posns, y)
			return nil
		})
	}
	group.Wait()
	close(lens)

	return <-longest
}

func max(in <-chan int, out chan<- int) {
	defer close(out)
	max := 0
	for v := range in {
		if v > max {
			max = v
		}
	}
	out <- max
}

func numContiguous(posns []lib.Point, y int) int {
	longest := 0
	n := 0
	in := false
	for x := 0; x < *width; x++ {
		p := lib.Point{x, y}
		if _, ok := slices.BinarySearchFunc(posns, p, lib.CmpPoint); ok {
			if in {
				n++
			} else {
				n = 1
				in = true
			}
		} else {
			in = false
		}
	}
	if n > longest {
		return n
	}
	return longest
}

func writeImg(posns []lib.Point, file string) error {
	img := image.NewAlpha16(image.Rect(0, 0, *width, *height))
	for i := range posns {
		img.SetAlpha16(posns[i].X, posns[i].Y, color.Opaque)
	}

	f, err := os.Create(file)
	if err != nil {
		return err
	}
	defer f.Close()

	return jpeg.Encode(f, img, &jpeg.Options{100})
}
