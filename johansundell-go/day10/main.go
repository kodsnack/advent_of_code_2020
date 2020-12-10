package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	input, _ := adventofcode2017.GetInput("day10.txt")
	strs := strings.Split(input, "\n")
	arr := make([]int, len(strs))
	for k, v := range strs {
		n, _ := strconv.Atoi(v)
		arr[k] = n
	}
	sort.Ints(arr)
	//fmt.Println(arr)
	//start := arr[0]
	no1, no3 := 0, 1
	for i, start := 0, 0; i < len(arr); i++ {
		switch arr[i] - start {
		case 1:
			no1++
		case 3:
			no3++
		}

		fmt.Println(arr[i], start, arr[i]-start)
		start = arr[i]
	}
	fmt.Println(no1, no3, no1*no3)
}
