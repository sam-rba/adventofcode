package main

import (
	"flag"
	"github.com/sam-rba/adventofcode/lib"
)

const steps = 100

const (
	widthDefault  = 101
	heightDefault = 103
)

var (
	width  = flag.Int("w", widthDefault, "grid width")
	height = flag.Int("h", heightDefault, "grid height")
)

type Robot struct {
	p lib.Point
	v lib.Vector
}

func (r *Robot) move(steps int) {
	r.p.X = mod(r.p.X+steps*r.v.X, *width)
	r.p.Y = mod(r.p.Y+steps*r.v.Y, *height)
}

func mod(a, b int) int {
	return (a%b + b) % b
}
