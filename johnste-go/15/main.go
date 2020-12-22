package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
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

	numbers := []int{}
	numberMap := map[int]int{}
	lastNum := 0
	for _, row := range data {
		for i, num := range strings.Split(row, ",") {
			lastNum, _ = strconv.Atoi(num)
			numbers = append(numbers, lastNum)
			numberMap[lastNum] = i + 1
		}
	}

	for i := len(numbers); i < 30000000; i++ {
		if val, ok := numberMap[lastNum]; ok {

			numberMap[lastNum] = i
			lastNum = i - val

		} else {

			numberMap[lastNum] = i
			lastNum = 0

		}
		numbers = append(numbers, lastNum)
	}

	fmt.Println("B", numbers[len(numbers)-1])
}
