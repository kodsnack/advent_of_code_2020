package main

import (
	"fmt"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, _ := adventofcode2017.GetInput("day6.txt")
	input := strings.Split(data, "\n\n")
	fmt.Println(countYes(input))
}

func countYes(input []string) (tot, totPart2 int) {
	for _, v := range input {
		str := strings.ReplaceAll(v, "\n", "")
		sizeOfGroup := len(strings.Split(v, "\n"))
		g := make(map[rune]int)
		for _, r := range str {
			g[r]++
		}
		i := 0
		for _, n := range g {
			if n == sizeOfGroup {
				i++
			}
		}
		totPart2 += i
		tot += len(g)
	}
	return
}
