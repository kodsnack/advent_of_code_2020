package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	//run("./example-data.txt")
	run("./data.txt")
}

func run(path string) {
	d := parse(path)
	result := a(d)
	fmt.Println(path, "A", result)
	result = b(d)
	fmt.Println(path, "B", result)
}

func a(data []string) uint {
	var sum uint = 0
	for _, row := range data {
		sum += unwrap(row, calculate)
	}
	return sum
}

func b(data []string) uint {
	var sum uint = 0
	for _, row := range data {
		num := unwrap(row, calculatePlus)
		fmt.Println(num, row)
		sum += num
	}
	return sum
}

func unwrap(s string, calc func(string) uint) uint {

	for {
		starts := []int{}
		finished := true

		for i := 0; i < len(s); i++ {
			if s[i] == '(' {
				starts = append(starts, i)
			} else if s[i] == ')' {

				start := starts[len(starts)-1]
				thing := s[start+1 : i]
				starts = starts[1:]
				num := calc(thing)

				s = s[:start] + fmt.Sprintf("%v", num) + s[i+1:]

				finished = false
				break
			}
		}

		if finished {
			break
		}
	}

	return calc(s)
}

func calculate(s string) uint {
	var sum uint = 0
	op := "+"

	for _, term := range strings.Split(s, " ") {

		if num, err := strconv.Atoi(term); err == nil {
			switch op {
			case "+":
				sum += uint(num)
			case "*":
				sum *= uint(num)
			}
		} else {
			switch term {
			case "+":
				op = "+"
			case "*":
				op = "*"
			}
		}
	}

	return sum
}

func calculatePlus(s string) uint {
	// var sum uint = 0
	// op := "+"

	r := regexp.MustCompile("(\\d+) \\+ (\\d+)")

	for {

		matches := r.FindStringSubmatch(s)
		matchesIndex := r.FindStringSubmatchIndex(s)

		if len(matches) == 0 {
			break
		}

		a, _ := strconv.Atoi(matches[1])
		b, _ := strconv.Atoi(matches[2])

		s = s[:matchesIndex[2]] + fmt.Sprintf("%v", a+b) + s[matchesIndex[5]:]

	}

	return calculate(s)
}

func parse(path string) []string {
	content, err := ioutil.ReadFile(path)
	check(err)
	return strings.Split(string(content), "\n")
}
