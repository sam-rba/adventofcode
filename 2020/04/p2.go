package main

import (
	"fmt"
	"strings"
	"strconv"
	"os"
	"bufio"
)

const (
	FNAME = "input"

	MIN_BYR = 1920
	MAX_BYR = 2002

	MIN_IYR = 2010
	MAX_IYR = 2020

	MIN_EYR = 2020
	MAX_EYR = 2030

	MIN_HGT_CM = 150
	MAX_HGT_CM = 193
	MIN_HGT_IN = 59
	MAX_HGT_IN = 76
)

var eyeColors = []string{"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}

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
		line, key, val string
		kv []string
		valid = 0
		passport Passport = Passport{}
		sc = bufio.NewScanner(file)
	)
	for lineNum := 1; sc.Scan(); lineNum++ {
		line = sc.Text()
		if line == "" {
			if passport.isValid() {
				valid++
			}
			passport = Passport{}
		}
		for _, field := range strings.Fields(line) {
			kv = strings.Split(field, ":")
			if len(kv) < 2 {
				fmt.Printf("Malformed field on line %d\n", lineNum)
				break
			}
			key = kv[0]
			val = kv[1]
			switch (key) {
			case "byr":
				passport.byr, err = isValidByr(val)
			case "iyr":
				passport.iyr, err = isValidIyr(val)
			case "eyr":
				passport.eyr, err = isValidEyr(val)
			case "hgt":
				passport.hgt, err = isValidHgt(val)
			case "hcl":
				passport.hcl = isValidHcl(val)
			case "ecl":
				passport.ecl = isValidEcl(val)
			case "pid":
				passport.pid = isValidPid(val)
			case "cid":
				passport.cid = true
			default:
				fmt.Printf("Unknown field on line %d: %s\n", lineNum, field)
				break;
			}
			if err != nil {
				fmt.Printf("Warning on line %d: %v\n", lineNum, err)
				err = nil
			}
		}
	}
	if passport.isValid() {
		valid++
	}
	fmt.Println(valid)
}

func isValidByr(val string) (bool, error) {
	byr, err := strconv.Atoi(val)
	if err != nil {
		return false, err
	}
	return MIN_BYR <= byr && byr <= MAX_BYR, nil
}

func isValidIyr(val string) (bool, error) {
	iyr, err := strconv.Atoi(val)
	if err != nil {
		return false, err
	}
	return MIN_IYR <= iyr && iyr <= MAX_IYR, nil
}

func isValidEyr(val string) (bool, error) {
	eyr, err := strconv.Atoi(val)
	if err != nil {
		return false, err
	}
	return MIN_EYR <= eyr && eyr <= MAX_EYR, nil
}

func isValidHgt(val string) (bool, error) {
	v, err := strconv.Atoi(val[:len(val)-2])
	if err != nil {
		return false, err
	}
	unit := val[len(val)-2:]
	switch (unit) {
	case "cm":
		return MIN_HGT_CM <= v && v <= MAX_HGT_CM, nil
	case "in":
		return MIN_HGT_IN <= v && v <= MAX_HGT_IN, nil
	default:
		return false, fmt.Errorf("Invalid distance unit: %s", unit)
	}
}

func isValidHcl(val string) bool {
	if len(val) != 7 {
		return false
	}
	if val[0] != '#' {
		return false
	}
	for _, c := range val[1:] {
		if !isHexDigit(c) {
			return false
		}
	}
	return true
}

func isHexDigit(c rune) bool {
	return ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')
}

func isValidEcl(val string) bool {
	for _, hcl := range eyeColors {
		if val == hcl {
			return true
		}
	}
	return false
}

func isValidPid(val string) bool {
	if len(val) != 9 {
		return false
	}
	for _, c := range val {
		if !isDigit(c) {
			return false
		}
	}
	return true
}

func isDigit(c rune) bool {
	return '0' <= c && c <= '9'
}
