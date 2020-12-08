package main

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type op struct {
	operation string
	argument  int
	hits      int
}

type instructions []op

func main() {
	data, _ := adventofcode2017.GetInput("day8.txt")
	p := parseInput(strings.Split(data, "\n"))
	fmt.Println(p.run())
	fmt.Println(p.fixProgram())
}

func (p instructions) fixProgram() int {
	for i := 0; i < len(p); i++ {
		t := make(instructions, len(p))
		p.reset()
		copy(t, p)
		switch t[i].operation {
		case "jmp":
			t[i].operation = "nop"
		case "nop":
			t[i].operation = "jmp"
		default:
			continue
		}
		result, err := t.run()
		if err == nil {
			return result
		}
	}
	return 0
}

func (program instructions) run() (tot int, err error) {
	for pos := 0; pos < len(program); {
		if program[pos].hits > 0 {
			return tot, errors.New("About to enter loop")
		}
		program[pos].hits++
		switch program[pos].operation {
		case "acc":
			tot = tot + program[pos].argument
			pos++
		case "jmp":
			pos = pos + program[pos].argument
		case "nop":
			pos++
		}
	}
	return tot, nil
}

func (p instructions) reset() {
	for i := 0; i < len(p); i++ {
		p[i].hits = 0
	}
}

func parseInput(data []string) instructions {
	program := []op{}
	for _, v := range data {
		f := strings.Fields(v)
		n, _ := strconv.Atoi(f[1])
		p := op{operation: f[0], argument: n}
		program = append(program, p)
	}
	return program
}
