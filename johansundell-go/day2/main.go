package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, _ := adventofcode2017.GetInput("day2.txt")
	pp := parseData(data)
	result1 := 0
	result2 := 0
	for _, p := range pp {
		if p.Validate() {
			result1++
		}
		if p.ValidateSecondSolution() {
			result2++
		}
	}
	fmt.Println(result1, result2)
}

func parseRawPassword(str []string) (p passwordPolicy) {
	minMax := strings.Split(str[0], "-")
	p.min, _ = strconv.Atoi(minMax[0])
	p.max, _ = strconv.Atoi(minMax[1])
	p.letter = strings.ReplaceAll(str[1], ":", "")
	p.password = str[2]
	return
}

func parseData(str string) (pp []passwordPolicy) {
	for _, v := range strings.Split(str, "\n") {
		p := parseRawPassword(strings.Split(v, " "))
		pp = append(pp, p)
	}
	return
}

type passwordPolicy struct {
	min      int
	max      int
	letter   string
	password string
}

func (p passwordPolicy) Validate() bool {
	num := strings.Count(p.password, p.letter)
	if num >= p.min && num <= p.max {
		return true
	}
	return false
}

func (p passwordPolicy) ValidateSecondSolution() bool {
	test1 := p.password[p.min-1:p.min] == p.letter
	test2 := p.password[p.max-1:p.max] == p.letter
	if test1 && !test2 {
		return true
	}
	if !test1 && test2 {
		return true
	}
	return false
}
