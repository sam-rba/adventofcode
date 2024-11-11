package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Parser struct {
	scanner *bufio.Scanner
	dot     *Dir
	cmds    []string
	entries []string
}

func parse(in io.Reader) (*Dir, error) {
	root := newDir("")
	p := Parser{bufio.NewScanner(in), root, nil, nil}

	for p.readLine() {
		fmt.Println("\nat ", p.dot.Name())
		if err := p.doCmd(); err != nil {
			return nil, err
		}
	}

	root, err := root.cd("/")
	if err == nil {
		delete(root.entries, "..")
	}
	return root, err
}

func (p *Parser) readLine() bool {
	if !p.scanner.Scan() {
		return false
	}

	line := p.scanner.Text()
	if strings.HasPrefix(line, "$ ") {
		p.cmds = append(p.cmds, line)
	} else {
		p.entries = append(p.entries, line)
	}
	return true
}

func (p *Parser) doCmd() error {
	var cmd string
	cmd, p.cmds = p.cmds[0], p.cmds[1:]

	if strings.HasPrefix(cmd, "$ cd") {
		arg := strings.TrimPrefix(cmd, "$ cd ")
		return p.mkcd(arg)
	} else if strings.HasPrefix(cmd, "$ ls") {
		fmt.Println("ls")
		p.readEntries()
		err := p.mkEntries()
		p.entries = []string{}
		return err
	}
	return fmt.Errorf("invalid command: %s", cmd)
}

func (p *Parser) mkcd(subdir string) error {
	if _, ok := p.dot.entries[subdir]; !ok {
		fmt.Println("mkdir", subdir)
		if err := p.dot.mkdir(subdir); err != nil {
			return err
		}
	}
	fmt.Println("cd", subdir)
	var err error
	p.dot, err = p.dot.cd(subdir)
	return err
}

func (p *Parser) readEntries() {
	for p.scanner.Scan() && !strings.HasPrefix(p.scanner.Text(), "$ ") {
		p.entries = append(p.entries, p.scanner.Text())
	}
	fmt.Println("Store cmd:", p.scanner.Text())
	p.cmds = append(p.cmds, p.scanner.Text())
}

func (p *Parser) mkEntries() error {
	for _, entry := range p.entries {
		if err := p.mkEntry(entry); err != nil {
			return err
		}
	}
	return nil
}

func (p *Parser) mkEntry(entry string) error {
	fields := strings.Split(entry, " ")
	if len(fields) != 2 {
		return fmt.Errorf("bad entry: %s", entry)
	}
	if fields[0] == "dir" {
		name := fields[1]
		fmt.Println("mkdir", name)
		return p.dot.mkdir(name)
	}
	return p.touch(fields[0], fields[1])
}

func (p *Parser) touch(sizeStr, name string) error {
	size, err := strconv.Atoi(sizeStr)
	if err != nil {
		return err
	}
	fmt.Printf("touch %s (%d B)\n", name, size)
	return p.dot.touch(name, size)
}
