package main

import (
	"fmt"
	"strings"
	"os"
	"bufio"
)

const FNAME = "input"

type Passport struct {
	byr bool
	iyr bool
	eyr bool
	hgt bool
	hcl bool
	ecl bool
	pid bool
	cid bool
}

func (pp Passport) isValid() bool {
	return pp.byr && pp.iyr && pp.eyr && pp.hgt && pp.hcl && pp.ecl && pp.pid
}

func main() {
	file, err := os.Open(FNAME)
	if err != nil {
		fmt.Printf("Error opening %s: %v\n", FNAME, err)
		os.Exit(1)
	}
	defer file.Close()

	var (
		line string
		valid = 0
		passport Passport = Passport{}
		sc = bufio.NewScanner(file)
	)
	for lineNum := 0; sc.Scan(); lineNum++ {
		line = sc.Text()
		if line == "" {
			if passport.isValid() {
				valid++
			}
			passport = Passport{}
		}
		for _, field := range strings.Fields(line) {
			field = strings.Split(field, ":")[0]
			switch (field) {
			case "byr":
				passport.byr = true
			case "iyr":
				passport.iyr = true
			case "eyr":
				passport.eyr = true
			case "hgt":
				passport.hgt = true
			case "hcl":
				passport.hcl = true
			case "ecl":
				passport.ecl = true
			case "pid":
				passport.pid = true
			case "cid":
				passport.cid = true
			default:
				fmt.Printf("Unknown field on line %d: %s\n", lineNum, field)
				break;
			}
		}
	}
	fmt.Println(valid)
}
