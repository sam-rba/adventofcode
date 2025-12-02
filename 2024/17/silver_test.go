package main

import "testing"

func TestBST(t *testing.T) {
	var c Computer
	c.reg[2] = 9
	prog := Program{2, 6}
	c.run(prog)
	if c.reg[1] != 1 {
		t.Error(c.reg[1])
	}
}

func TestTen(t *testing.T) {
	var c Computer
	c.reg[0] = 10
	prog := Program{5, 0, 5, 1, 5, 4}
	c.run(prog)
	if c.print() != "0,1,2" {
		t.Error(c.print())
	}
}

func TestYear(t *testing.T) {
	var c Computer
	c.reg[0] = 2024
	prog := Program{0, 1, 5, 4, 3, 0}
	c.run(prog)
	if c.print() != "4,2,5,6,7,7,7,7,3,1,0" {
		t.Error(c.print())
	}
}

func TestBXL(t *testing.T) {
	var c Computer
	c.reg[1] = 29
	prog := Program{1, 7}
	c.run(prog)
	if c.reg[1] != 26 {
		t.Error(c.reg[1])
	}
}

func TestBXC(t *testing.T) {
	var c Computer
	c.reg[1] = 2024
	c.reg[2] = 43690
	prog := Program{4, 0}
	c.run(prog)
	if c.reg[1] != 44354 {
		t.Error(c.reg[1])
	}
}
