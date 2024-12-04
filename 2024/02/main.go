package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/workpool"
	"io"
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	minStep = 1
	maxStep = 3
)

type Report struct {
	levels []int
}

type Step struct {
	curr, prev int
}

func (s Step) size() int {
	return s.curr - s.prev
}

func forward(s []int) <-chan Step {
	c := make(chan Step)
	go func() {
		defer close(c)
		for i := 1; i < len(s); i++ {
			c <- Step{s[i], s[i-1]}
		}
	}()
	return c
}

func reverse(s []int) <-chan Step {
	c := make(chan Step)
	go func() {
		defer close(c)
		for i := len(s) - 2; i >= 0; i-- {
			c <- Step{s[i], s[i+1]}
		}
	}()
	return c
}

func main() {
	reports := make(chan Report)
	go parse(os.Stdin, reports)
	fmt.Println("silver:", countSafe(reports))
}

func parse(in io.Reader, out chan<- Report) {
	defer close(out)
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		report, err := parseReport(scanner.Text())
		if err != nil {
			log.Fatal("bad input:", scanner.Text())
		}
		out <- report
	}
}

func parseReport(str string) (Report, error) {
	levels := make([]int, 0, len(str)/2+1)
	fields := strings.Fields(str)
	for i := range fields {
		level, err := strconv.Atoi(fields[i])
		if err != nil {
			return Report{}, err
		}
		levels = append(levels, level)
	}
	return Report{levels}, nil
}

func countSafe(reports <-chan Report) int {
	safe := make(chan int)
	numSafe := make(chan int)
	go count(safe, numSafe)

	pool := workpool.New(workpool.DefaultSize)
	for report := range reports {
		pool.Spawn(func() {
			if report.isSafe() {
				safe <- 1
			}
		})
	}
	pool.Wait()
	pool.Close()
	close(safe)

	return <-numSafe
}

func count[T any](in <-chan T, num chan<- int) {
	defer close(num)
	n := 0
	for range in {
		n++
	}
	num <- n
}
