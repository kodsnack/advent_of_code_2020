package main

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type passport struct {
	byr string
	iyr string
	eyr string
	hgt string
	hcl string
	ecl string
	pid string
	cid string
}

func newPassport(str string) (p passport) {
	str = strings.ReplaceAll(str, "\n", " ")
	data := strings.Split(str, " ")
	for _, v := range data {
		switch v[0:3] {
		case "byr":
			p.byr = v[4:]
		case "iyr":
			p.iyr = v[4:]
		case "eyr":
			p.eyr = v[4:]
		case "hgt":
			p.hgt = v[4:]
		case "hcl":
			p.hcl = v[4:]
		case "ecl":
			p.ecl = v[4:]
		case "pid":
			p.pid = v[4:]
		case "cid":
			p.cid = v[4:]
		}
	}
	return
}

func (p passport) isValid() bool {
	if p.byr != "" && p.iyr != "" && p.eyr != "" && p.hgt != "" && p.hcl != "" && p.ecl != "" && p.pid != "" {
		return true
	}
	return false
}

func (p passport) isValidPart2() bool {
	byr, err := strconv.Atoi(p.byr)
	if err != nil {
		return false
	}
	if byr < 1920 || byr > 2002 {
		return false
	}

	iyr, err := strconv.Atoi(p.iyr)
	if err != nil {
		return false
	}
	if iyr < 2010 || iyr > 2020 {
		return false
	}

	eyr, err := strconv.Atoi(p.eyr)
	if err != nil {
		return false
	}
	if eyr < 2020 || eyr > 2030 {
		return false
	}

	if len(p.hgt) < 4 {
		return false
	}
	htgType := p.hgt[len(p.hgt)-2:]
	htg, err := strconv.Atoi(p.hgt[:len(p.hgt)-2])
	if err != nil {
		return false
	}
	switch htgType {
	case "cm":
		if htg < 150 || htg > 193 {
			return false
		}
	case "in":
		if htg < 59 || htg > 76 {
			return false
		}
	default:
		return false
	}

	if m, err := regexp.MatchString("^#[0-9a-f]{6}$", p.hcl); err != nil || !m {
		return false
	}

	em := []string{"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}
	found := false
	for _, v := range em {
		if v == p.ecl {
			found = true
		}
	}
	if !found {
		return false
	}

	if len(p.pid) != 9 {
		return false
	}
	_, err = strconv.Atoi(p.pid)
	if err != nil {
		return false
	}
	return true
}

func main() {
	input, _ := adventofcode2017.GetInput("day4.txt")
	data := strings.Split(input, "\n\n")
	noValid, noValidPart2 := 0, 0
	for _, v := range data {
		p := newPassport(v)
		if p.isValid() {
			noValid++
			if p.isValidPart2() {
				noValidPart2++
			}
		}
	}
	fmt.Println(noValid, noValidPart2)
}
