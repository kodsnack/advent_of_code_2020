package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func readFile(path string) []string {
	file, err := os.Open(path)
	defer file.Close()
	check(err)

	result := []string{}
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		result = append(result, line)
	}

	return result
}

type instruction struct {
	op       string
	number   int
	numtimes int
}

func main() {

	data := readFile("./data.txt")

	instructions := []instruction{}

	for _, value := range data {
		r := regexp.MustCompile("^(\\w+) ([-+])(\\d+)$")
		matches := r.FindStringSubmatch(value)

		op := matches[1]
		sign := matches[2]
		num, _ := strconv.Atoi(matches[3])
		if sign == "-" {
			num = -num
		}

		instructions = append(instructions, instruction{op, num, 0})
	}

	// A

	instructionsCopy := make([]instruction, len(instructions))
	copy(instructionsCopy, instructions)
	_, acc := run(instructionsCopy)

	fmt.Println("A", acc)

	// B

	for x := 0; x < len(instructions); x++ { // ++ is a statement.
		instructionsCopy := make([]instruction, len(instructions))
		copy(instructionsCopy, instructions)

		instruction := &instructionsCopy[x]
		if instruction.op == "nop" {
			instruction.op = "jmp"
		} else if instruction.op == "jmp" {
			instruction.op = "nop"
		}

		success, acc := run(instructionsCopy)

		if success {
			fmt.Println("B", acc)
		}
	}

}

func run(instructions []instruction) (bool, int) {

	acc := 0
	pc := 0

	for instructions[pc].numtimes < 1 {

		instruction := &instructions[pc]

		instruction.numtimes += 1

		switch instruction.op {
		case "nop":
			pc += 1
		case "acc":
			acc += instruction.number
			pc += 1
		case "jmp":
			pc += instruction.number
		}

		if pc > len(instructions)-1 {
			return true, acc
		}

	}

	return false, acc
}
