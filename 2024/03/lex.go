// Based on Rob Pike's talk on the lexical scanner in the Go template package.
package main

import (
	"fmt"
	"strings"
	"unicode/utf8"
)

const (
	doInstr   = "do"
	dontInstr = "don't"
	mulInstr  = "mul"
)

const eof = -1

type item struct {
	typ itemType
	val string
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError  itemType = iota // error occured; value is text of error
	itemEOF                    // end of file
	itemDo                     // do instruction
	itemDont                   // don't instruction
	itemMul                    // mul instruction
	itemOpen                   // opening parenthesis '('
	itemClose                  // closing parenthesis ')'
	itemComma                  // comma ','
	itemNumber                 // number
	itemText                   // garbage text
)

func (i item) String() string {
	switch i.typ {
	case itemEOF:
		return "EOF"
	case itemError:
		return i.val
	}
	if len(i.val) > 10 {
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// lexStateFn represents the state of the scanner
// as a function that returns the next state.
type lexStateFn func(*lexer) lexStateFn

// lexer holds the state of the scanner.
type lexer struct {
	input string    // the string being scanned.
	start int       // start position of this item.
	pos   int       // current position in the input.
	width int       // width of last rune read from unput.
	items chan item // channel of scanned items.
}

func lex(input string) <-chan item {
	l := &lexer{
		input: input,
		items: make(chan item),
	}
	go l.run() // Concurrently run state machine.
	return l.items
}

// run lexes the input by executing state functions until
// the state is nil.
func (l *lexer) run() {
	for state := lexText; state != nil; {
		state = state(l)
	}
	close(l.items) // No more tokens will be delivered.
}

func (l *lexer) emit(t itemType) {
	l.items <- item{t, l.input[l.start:l.pos]}
	l.start = l.pos
}

// next returns the next rune in the input.
func (l *lexer) next() rune {
	if l.pos >= len(l.input) {
		l.width = 0
		return eof
	}
	var r rune
	r, l.width = utf8.DecodeRuneInString(l.input[l.pos:])
	l.pos += l.width
	return r
}

// ignore skips over the pending input before this point.
func (l *lexer) ignore() {
	l.start = l.pos
}

// backup steps back one rune.
// Can be called only once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width
}

// peek returns but does not consume
// the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// accept consumes the next rune
// if it's from the valid set.
func (l *lexer) accept(valid string) bool {
	if strings.IndexRune(valid, l.next()) >= 0 {
		return true
	}
	l.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) {
	for strings.IndexRune(valid, l.next()) >= 0 {
	}
	l.backup()
}

// error returns an error token and terminates the scan
// by passing back a nil pointer that will be the next
// state, terminating l.run.
func (l *lexer) errorf(format string, args ...interface{}) lexStateFn {
	l.items <- item{
		itemError,
		fmt.Sprintf(format, args...),
	}
	return nil
}

func lexText(l *lexer) lexStateFn {
	for {
		if strings.HasPrefix(l.input[l.pos:], dontInstr) {
			if l.pos > l.start {
				l.emit(itemText)
			}
			return lexDont // Next state
		} else if strings.HasPrefix(l.input[l.pos:], doInstr) {
			if l.pos > l.start {
				l.emit(itemText)
			}
			return lexDo // Next state.

		} else if strings.HasPrefix(l.input[l.pos:], mulInstr) {
			if l.pos > l.start {
				l.emit(itemText)
			}
			return lexMul // Next state
		}
		if l.next() == eof {
			break
		}
	}
	// Correctly reached EOF.
	if l.pos > l.start {
		l.emit(itemText)
	}
	l.emit(itemEOF)
	return nil // Stop the run loop.
}

func lexDo(l *lexer) lexStateFn {
	l.pos += len(doInstr)
	if l.input[l.pos] == '(' {
		l.emit(itemDo)
		return lexOpen
	}
	return lexText
}

func lexDont(l *lexer) lexStateFn {
	fmt.Println("lexDont")
	l.pos += len(dontInstr)
	if l.input[l.pos] == '(' {
		l.emit(itemDont)
		return lexOpen
	}
	return lexText
}

func lexMul(l *lexer) lexStateFn {
	l.pos += len(mulInstr)
	if l.input[l.pos] == '(' {
		l.emit(itemMul)
		return lexOpen
	}
	return lexText
}

func lexOpen(l *lexer) lexStateFn {
	l.pos += len("(")
	l.emit(itemOpen)
	return lexArgs
}

func lexArgs(l *lexer) lexStateFn {
	switch r := l.next(); {
	case '0' <= r && r <= '9':
		l.backup()
		return lexNumber
	case r == ',':
		l.backup()
		return lexComma
	case r == ')':
		l.backup()
		return lexClose
	default:
		return lexText
	}
}

func lexNumber(l *lexer) lexStateFn {
	l.acceptRun("0123456789")
	l.emit(itemNumber)
	return lexArgs
}

func lexComma(l *lexer) lexStateFn {
	l.pos += len(",")
	l.emit(itemComma)
	return lexArgs
}

func lexClose(l *lexer) lexStateFn {
	l.pos += len(")")
	l.emit(itemClose)
	return lexText
}
