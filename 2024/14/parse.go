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

func parse(in io.Reader, out chan<- Robot) {
	defer close(out)
	scanner := bufio.NewScanner(in)
	for scanner.Scan() {
		robot, err := parseRobot(scanner.Text())
		if err != nil {
			log.Fatal(err)
		}
		out <- robot
	}
}

func parseRobot(s string) (Robot, error) {
	fields := strings.Fields(s)
	if len(fields) != 2 {
		return Robot{}, fmt.Errorf("bad robot: '%s'", s)
	}

	ps := strings.TrimPrefix(fields[0], "p=")
	p, err := parsePoint(ps)
	if err != nil {
		return Robot{}, fmt.Errorf("bad robot '%s': %v", s, err)
	}

	vs := strings.TrimPrefix(fields[1], "v=")
	v, err := parsePoint(vs)
	if err != nil {
		return Robot{}, fmt.Errorf("bad robot '%s': %v", s, err)
	}

	return Robot{p, lib.Vector{v.X, v.Y}}, nil
}

func parsePoint(s string) (lib.Point, error) {
	fields := strings.Split(s, ",")
	if len(fields) != 2 {
		return lib.Point{}, fmt.Errorf("bad point: '%s'", s)
	}

	x, err := strconv.Atoi(fields[0])
	if err != nil {
		return lib.Point{}, err
	}

	y, err := strconv.Atoi(fields[1])

	return lib.Point{x, y}, err
}
