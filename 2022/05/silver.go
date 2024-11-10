package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Stack []byte

func (st Stack) push(c byte) Stack {
	return append(st, c)
}

func (st Stack) pop() (Stack, byte, bool) {
	if len(st) <= 0 {
		return st, 0, false
	}
	c := st[len(st)-1]
	st = st[:len(st)-1]
	return st, c, true
}

type Move struct {
	n        int
	from, to int
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	stacks := parseStacks(scanner)

	moves := make(chan Move)
	go parseMoves(scanner, moves)
	for mv := range moves {
		fmt.Println()
		for _, st := range stacks {
			fmt.Println(st)
		}

		move(stacks, mv)
	}

	fmt.Println("silver:", msg(stacks))
}

func parseStacks(in *bufio.Scanner) []Stack {
	var buf [][]byte

	for in.Scan() && in.Text() != "" {
		buf = append(buf, []byte(in.Text()))
	}

	nStacks := len(buf[len(buf)-1]) / 3
	stacks := make([]Stack, nStacks)
	buf = buf[:len(buf)-1]

	for y := len(buf) - 1; y >= 0; y-- {
		line := buf[y]
		s := 0
		for i := 1; i < len(line); i += 4 {
			if line[i] != ' ' {
				stacks[s] = stacks[s].push(line[i])
			}
			s++
		}
	}

	return stacks
}

func parseMoves(in *bufio.Scanner, out chan<- Move) {
	defer close(out)
	for in.Scan() {
		move, err := parseMove(in.Text())
		if err != nil {
			log.Fatal(err)
		}
		out <- move
	}
}

func parseMove(str string) (Move, error) {
	orig := str
	str = strings.TrimPrefix(str, "move ")
	fields := strings.Split(str, " from ")
	if len(fields) != 2 {
		return Move{}, MalformedMove{orig}
	}
	str = fields[1]

	n, err := strconv.Atoi(fields[0])
	if err != nil {
		return Move{}, err
	}

	fields = strings.Split(str, " to ")
	if len(fields) != 2 {
		return Move{}, MalformedMove{orig}
	}

	from, err := strconv.Atoi(fields[0])
	if err != nil {
		return Move{}, MalformedMove{orig}
	}

	to, err := strconv.Atoi(fields[1])

	return Move{n, from - 1, to - 1}, err
}

func move(stacks []Stack, move Move) {
	from := stacks[move.from]
	to := stacks[move.to]
	for ; move.n > 0; move.n-- {
		var c byte
		from, c, _ = from.pop()
		to = to.push(c)
	}
	stacks[move.from] = from
	stacks[move.to] = to
}

func msg(stacks []Stack) string {
	msg := make([]byte, 0, len(stacks))
	for _, st := range stacks {
		_, c, _ := st.pop()
		msg = append(msg, c)
	}
	return string(msg)
}

type MalformedMove struct {
	str string
}

func (e MalformedMove) Error() string {
	return fmt.Sprint("malformed move:", e.str)
}
