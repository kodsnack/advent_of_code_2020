package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, err := adventofcode2017.GetInput("day1.txt")
	if err != nil {
		panic(err)
	}
	arr := strings.Split(data, "\n")
	for _, v := range arr {
		for _, x := range arr {
			a, _ := strconv.Atoi(v)
			b, _ := strconv.Atoi(x)
			if a+b == 2020 {
				fmt.Println(a, b, a*b)
			}
			for _, y := range arr {
				c, _ := strconv.Atoi(y)
				if a+b+c == 2020 {
					fmt.Println(a, b, c, a*b*c)
				}
			}
		}
	}
}
