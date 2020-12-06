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

func countYes(input []string) (tot int) {
	groups := make([]map[rune]struct{}, 0)
	for _, v := range input {
		str := strings.ReplaceAll(v, "\n", "")
		g := make(map[rune]struct{})
		for _, r := range str {
			g[r] = struct{}{}
		}
		groups = append(groups, g)
	}
	for _, g := range groups {
		tot += len(g)
	}
	return
}
