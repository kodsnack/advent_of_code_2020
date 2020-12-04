package main

import (
	"fmt"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

const treeSign = `#`

type slope struct {
	mapData []string
}

func (s slope) hasTree(y, x int) bool {
	x = x % len(s.mapData[0])
	if s.mapData[y][x:x+1] == treeSign {
		return true
	}
	return false
}

func (s slope) travel(mY, mX int) (trees int) {
	y, x := 0, 0
	for y < len(s.mapData) {
		if s.hasTree(y, x) {
			trees++
		}
		x += mX
		y += mY
	}
	return
}

func main() {
	data, _ := adventofcode2017.GetInput("day3.txt")
	s := slope{mapData: strings.Split(data, "\n")}
	fmt.Println(s.travel(1, 3))
	fmt.Println(s.travel(1, 1) * s.travel(1, 3) * s.travel(1, 5) * s.travel(1, 7) * s.travel(2, 1))
}
