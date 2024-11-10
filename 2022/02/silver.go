package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
)

type Round struct {
	you, opponent Hand
}

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

func (r Round) score() int {
	return int(r.outcome()) + int(r.you)
}

func (r Round) outcome() Outcome {
	if r.you.beats(r.opponent) {
		return win
	} else if r.opponent.beats(r.you) {
		return loss
	}
	return draw
}

func (h Hand) beats(other Hand) bool {
	return other%3 == h-1
}

func main() {
	rounds := make(chan Round)
	go parseRounds(os.Stdin, rounds)
	score := 0
	for round := range rounds {
		fmt.Println(round, round.score())
		score += round.score()
	}
	fmt.Println("silver:", score)
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

func parseRound(str string) (Round, error) {
	if len(str) != 3 || str[0] < 'A' || str[0] > 'C' || str[2] < 'X' || str[2] > 'Z' {
		return Round{}, MalformedRound{str}
	}
	return Round{
		opponent: Hand(str[0] - 'A' + 1),
		you: Hand(str[2] - 'X' + 1),
	}, nil
}

type MalformedRound struct {
	round string
}

func (e MalformedRound) Error() string {
	return fmt.Sprintf("malformed round: '%s'", e.round)
}

func (r Round) String() string {
	return fmt.Sprintf("{%v, %v}", r.you, r.opponent)
}

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
