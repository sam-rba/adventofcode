package main

import (
	"bufio"
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"io"
	"log"
	"strconv"
	"strings"
)

func parse(in io.Reader, out chan<- Machine) {
	defer close(out)
	scanner := bufio.NewScanner(in)
	for {
		machine, err := parseMachine(scanner)
		if err == io.EOF {
			return
		} else if err != nil {
			log.Fatal(err)
		}
		out <- machine

		if !scanner.Scan() {
			break
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

func parseMachine(in *bufio.Scanner) (Machine, error) {
	if !in.Scan() {
		err := in.Err()
		if err == nil {
			err = io.EOF
		}
		return Machine{}, err
	}
	s := strings.TrimPrefix(in.Text(), "Button A: ")
	da, err := parseVec(s)
	if err != nil {
		return Machine{}, err
	}

	if !in.Scan() {
		err := in.Err()
		if err == nil {
			return Machine{}, fmt.Errorf("unexpected EOF")
		}
		return Machine{}, err
	}
	s = strings.TrimPrefix(in.Text(), "Button B: ")
	db, err := parseVec(s)
	if err != nil {
		return Machine{}, err
	}

	if !in.Scan() {
		err := in.Err()
		if err == nil {
			return Machine{}, fmt.Errorf("unexpected EOF")
		}
		return Machine{}, err
	}
	s = strings.TrimPrefix(in.Text(), "Prize: ")
	prize, err := parsePoint(s)
	return Machine{da, db, prize}, err
}

func parseVec(s string) (lib.Vector, error) {
	fields := strings.Split(s, ", ")
	if len(fields) != 2 {
		return lib.Vector{}, fmt.Errorf("bad vector: %s", s)
	}

	x, err := strconv.Atoi(strings.TrimPrefix(fields[0], "X+"))
	if err != nil {
		return lib.Vector{}, fmt.Errorf("bad vector '%s': %v", s, err)
	}

	y, err := strconv.Atoi(strings.TrimPrefix(fields[1], "Y+"))
	if err != nil {
		return lib.Vector{}, fmt.Errorf("bad vector: '%s': %v", s, err)
	}

	return lib.Vector{x, y}, nil
}

func parsePoint(s string) (lib.Point, error) {
	v, err := parseVec(strings.ReplaceAll(s, "=", "+"))
	return lib.Point{v.X, v.Y}, err
}
