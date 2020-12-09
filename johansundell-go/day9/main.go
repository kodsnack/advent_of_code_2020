package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

type preamble []int

func main() {
	str, _ := adventofcode2017.GetInput("day9.txt")
	data := strings.Split(str, "\n")
	input := make([]int, len(data))
	for k, v := range data {
		n, _ := strconv.Atoi(v)
		input[k] = n
	}
	p1 := part1(input, 25)
	p2 := part2(input, p1)
	fmt.Println(p1, p2)
}

func NewPreamble(input []int) preamble {
	p := preamble{}
	for _, v := range input {
		p = append(p, v)
	}
	return p
}

func (p *preamble) Add(n int) {
	*p = append((*p)[1:], n)
}

func (p preamble) HasSum(num int) bool {
	for i := 0; i < len(p); i++ {
		for n := 0; n < len(p); n++ {
			if i == n {
				continue
			}
			if p[i]+p[n] == num {
				return true
			}
		}
	}
	return false
}

func part1(input []int, num int) int {
	p := NewPreamble(input[:num])
	for _, v := range input[num:] {
		if !p.HasSum(v) {
			return v
		}
		p.Add(v)
	}
	return -1
}

func part2(input []int, num int) int {
	for n := range input {
		res := make([]int, 0)
		for i, sum := n, 0; i < len(input); i++ {
			res = append(res, input[i])
			sum += input[i]
			if sum == num {
				sort.Ints(res)
				return res[0] + res[len(res)-1]
			}
		}
	}
	return -1
}
