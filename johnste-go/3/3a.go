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

func readFile(path string, process func(int, string) string) []string {
	file, err := os.Open(path)
	defer file.Close()

	check(err)

	result := []string{}
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		result = append(result, process(0, scanner.Text()))
	}

	return result
}

func isTree(data []string, x int, y int, width int) bool {
	return data[y][x%width] == '#'
}

type slope struct {
	x, y int
}

func main() {

	data := readFile("./data.txt", func(i int, text string) string {
		return text
	})

	width := len(data[0])
	trees := 0
	x := 0

	// 3A
	for y, _ := range data {
		if isTree(data, x, y, width) {
			trees += 1
		}
		x += 3
	}

	fmt.Printf("3A Answer: %d\n", trees)

	// 3B
	slopes := []slope{
		{1, 1},
		{3, 1},
		{5, 1},
		{7, 1},
		{1, 2},
	}

	allSlopes := []int{}

	for _, slope := range slopes {
		trees := 0
		x := 0
		y := 0
		for y < len(data) {
			if isTree(data, x, y, width) {
				trees += 1
			}
			x += slope.x
			y += slope.y
		}

		allSlopes = append(allSlopes, trees)
	}

	product := 1
	for _, num := range allSlopes {
		product *= num
	}
	fmt.Printf("3B Answer: %d\n", product)
}
