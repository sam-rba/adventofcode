package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
)

type Hand int

const (
	_ Hand = iota
	rock
	paper
	scissors
)

type Outcome int

const (
	loss Outcome = 3 * iota
	draw
	win
)

func (h Hand) String() string {
	switch h {
	case rock:
		return "R"
	case paper:
		return "P"
	case scissors:
		return "S"
	}
	panic(fmt.Sprintf("illegal hand: %d", h))
}

func (o Outcome) String() string {
	switch o {
	case win:
		return "W"
	case loss:
		return "L"
	case draw:
		return "D"
	}
	panic(fmt.Sprintf("illegal outcome: %d", o))
}

func parseRounds(in io.Reader, out chan<- Round) {
	defer close(out)
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		round, err := parseRound(scanner.Text())
		if err != nil {
			log.Fatal(err)
		}
		out <- round
	}
}

type MalformedRound struct {
	round string
}

func (e MalformedRound) Error() string {
	return fmt.Sprintf("malformed round: '%s'", e.round)
}
