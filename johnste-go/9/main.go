package main

import (
	"bufio"
	"fmt"
	"os"
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
	preamble := 25

	history := []int{}
	completeHistory := []int{}

	invalidNumber := 0
	for _, value := range data {
		num, _ := strconv.Atoi(value)

		completeHistory = append(completeHistory, num)

		if len(history) > preamble {
			_, history = history[0], history[1:]

			if !findSum(num, history) {
				invalidNumber = num
				fmt.Println("A", num)
				break
			}

		}

		history = append(history, num)

	}

	fmt.Println("B", findRange(invalidNumber, completeHistory))
}

func findSum(target int, history []int) bool {

	for _, a := range history {
		for _, b := range history {
			if a != b && a+b == target {
				return true
			}
		}
	}

	return false

}

func findRange(target int, history []int) int {

	for i := 0; i < len(history); i++ {
		for j := 0; j < len(history); j++ {

			if j <= i {
				continue
			}

			sum := 0
			min := history[i]
			max := history[i]
			for _, num := range history[i:j] {
				sum += num
				if num > max {
					max = num
				}
				if num < min {
					min = num
				}
			}

			if sum == target {
				return min + max
			}

		}
	}
	return 0
}
