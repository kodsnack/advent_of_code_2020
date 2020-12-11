package main

import (
	"bufio"
	"fmt"
	"os"
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

func main() {

	data := readFile("./data.txt")

	// A
	questions := map[string]int{}
	sum := 0
	for _, value := range data {
		if value == "" {
			sum += len(questions)
			questions = map[string]int{}
			continue
		}

		for _, char := range value {
			questions[string(char)] = 1
		}
	}

	sum += len(questions)
	fmt.Printf("A: %d\n", sum)

	// B
	questions = map[string]int{}
	sum = 0
	numPeople := 0
	for _, value := range data {
		if value == "" {
			sum += countAll(numPeople, questions)
			questions = map[string]int{}
			numPeople = 0
			continue
		}

		numPeople += 1
		for _, char := range value {
			if _, ok := questions[string(char)]; ok {
				questions[string(char)] += 1
			} else {
				questions[string(char)] = 1
			}
		}
	}
	sum += countAll(numPeople, questions)
	fmt.Printf("B: %d\n", sum)
}

func countAll(numPeople int, questions map[string]int) int {
	all := 0

	for _, value := range questions {
		if numPeople == value {
			all += 1
		}
	}
	fmt.Printf("np: %d, to %d\n", numPeople, all)
	return all
}
