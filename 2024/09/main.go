package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"os"
	"unicode"
)

// Cluster is a contiguous group of blocks with the same ID number.
type Cluster struct {
	id          int
	start, size int
	free        bool
}

type Disc []Cluster

func main() {
	disc, err := parseDisc(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	disc = disc.compact()

	fmt.Printf("%s: %d\n", part, disc.checksum())
}

func parseDisc(in io.Reader) (Disc, error) {
	var (
		disc          Disc
		err           error
		pos, size, id int
	)

	r := bufio.NewReader(in)
	for {
		// File.
		size, err = readSize(r)
		if err != nil {
			break
		}
		disc = append(disc, Cluster{
			id,
			pos,
			size,
			false,
		})
		id++
		pos += size

		// Free space.
		size, err = readSize(r)
		if err != nil {
			break
		}
		disc = append(disc, Cluster{
			-1,
			pos,
			size,
			true,
		})
		pos += size
	}

	if err == io.EOF {
		err = nil
	}
	return disc, err
}

func readSize(r io.ByteReader) (int, error) {
	b, err := r.ReadByte()
	if err != nil {
		return -1, err
	}
	if b == '\n' {
		return -1, io.EOF
	}
	return parseSize(b)
}

func parseSize(b byte) (int, error) {
	if !unicode.IsDigit(rune(b)) {
		return -1, fmt.Errorf("invalid cluster size: %c", b)
	}
	return int(b - '0'), nil
}

func (disc Disc) trim() Disc {
	if disc[len(disc)-1].free {
		return disc[:len(disc)-1]
	}
	return disc
}

func (d Disc) checksum() int {
	chks := 0
	for _, cluster := range d {
		chks += cluster.checksum()
	}
	return chks
}

func (c Cluster) checksum() int {
	if c.free {
		return 0
	}

	chks := 0
	for i := c.start; i < c.start+c.size; i++ {
		chks += i
	}
	return chks * c.id
}
