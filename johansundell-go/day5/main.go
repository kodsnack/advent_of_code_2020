package main

import (
	"fmt"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type seat struct {
	row    int
	column int
}

func (s seat) getId() int {
	return (s.row * 8) + s.column
}

func newSeat(str string) (s seat) {
	y0, y1 := 0, 128
	for _, v := range str[:7] {
		y := (y1 - y0) / 2
		switch v {
		case 'F':
			y1 -= y
		case 'B':
			y0 += y
		}
	}
	s.row = y0

	x0, x1 := 0, 8
	for _, v := range str[7:] {
		x := (x1 - x0) / 2
		switch v {
		case 'L':
			x1 -= x
		case 'R':
			x0 += x
		}
	}
	s.column = x1 - 1
	return
}

func main() {
	input, _ := adventofcode2017.GetInput("day5.txt")
	data := strings.Split(input, "\n")
	max := 0
	list := make(map[seat]int)
	for _, v := range data {
		s := newSeat(v)
		list[s] = s.getId()
		if s.getId() > max {
			max = s.getId()
		}
	}
	fmt.Println(max)
	for y := 0; y < 128; y++ {
		for x := 0; x < 8; x++ {
			ts := seat{row: y, column: x}
			_, isThere := list[ts]
			if !isThere {
				fmt.Println(y, x, ts.getId())
			}
		}
	}
}
