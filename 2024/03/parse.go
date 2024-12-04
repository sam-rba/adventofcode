package main

import (
	"fmt"
	"strconv"
)

type parser struct {
	items        <-chan item
	instructions chan<- Instruction
	left, right  int // operands.
}

func parse(items <-chan item) <-chan Instruction {
	instructions := make(chan Instruction)
	itms := make(chan item)
	go func() {
		for itm := range items {
			fmt.Println("item", itm)
			itms <- itm
		}
		close(itms)
	}()
	p := &parser{
		items:        itms,
		instructions: instructions,
	}
	go p.run()
	return instructions
}

func (p *parser) run() {
	for state := parseText; state != nil; {
		state = state(p)
	}
	close(p.instructions)
}

type parseStateFn func(*parser) parseStateFn

func (p *parser) errorf(format string, args ...interface{}) parseStateFn {
	fmt.Printf(format, args)
	return nil
}

func parseText(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	switch itm.typ {
	case itemDo:
		return parseDoOpen
	case itemDont:
		return parseDontOpen
	case itemMul:
		return parseMulOpen
	default:
		return parseText
	}
}

func parseDoOpen(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemOpen {
		return parseDoClose
	}
	return parseText
}

func parseDoClose(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemClose {
		p.instructions <- Do{}
	}
	return parseText
}

func parseDontOpen(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemOpen {
		return parseDontClose
	}
	return parseText
}

func parseDontClose(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemClose {
		p.instructions <- Dont{}
	}
	return parseText
}

func parseMulOpen(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemOpen {
		return parseMulLeft
	}
	return parseText
}

func parseMulLeft(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemNumber {
		n, err := strconv.Atoi(itm.val)
		if err != nil {
			return p.errorf("bad left operand '%s': %v", itm.val, err)
		}
		p.left = n
		return parseMulComma
	}
	return parseText
}

func parseMulComma(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemComma {
		return parseMulRight
	}
	return parseText
}

func parseMulRight(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemNumber {
		n, err := strconv.Atoi(itm.val)
		if err != nil {
			return p.errorf("bad right operand '%s': %v", itm.val, err)
		}
		p.right = n
		return parseMulClose
	}
	return parseText
}

func parseMulClose(p *parser) parseStateFn {
	itm, ok := <-p.items
	if !ok {
		return nil
	}
	if itm.typ == itemClose {
		p.instructions <- Mul{p.left, p.right}
	}
	return parseText
}
