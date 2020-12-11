package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
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

type node struct {
	number   int
	children []node
}

func main() {

	data := readFile("./data.txt")

	numbers := []int{}

	for _, value := range data {
		num, _ := strconv.Atoi(value)
		numbers = append(numbers, num)
	}
	numbers = append(numbers, 0)
	sort.Ints(numbers)

	oneDiff := 0
	threeDiff := 1
	lastVal := 0
	for _, value := range numbers {
		if value-lastVal == 3 {
			threeDiff += 1
		}
		if value-lastVal == 1 {
			oneDiff += 1
		}
		lastVal = value
	}

	fmt.Println("A", oneDiff*threeDiff)

	memo := map[int]int{}
	target := numbers[len(numbers)-1]
	fmt.Println("can reach", canReach(target, numbers, &memo, 0))
}

func canReach(number int, numbers []int, memo *map[int]int, depth int) int {
	if number == 0 {
		return 1
	}

	sum := 0
	for i := len(numbers) - 1; i >= 0; i-- {
		value := numbers[i]
		if number-value >= 1 && number-value <= 3 {
			if val, ok := (*memo)[value]; ok {
				sum += val
			} else {
				res := canReach(value, numbers, memo, depth+1)
				sum += res
				(*memo)[value] = res
			}
		}
	}

	return sum
}
