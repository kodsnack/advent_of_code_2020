package main

import (
	"strconv"
	"strings"
	"testing"
)

const testData = `35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576`

func Test_Ex1(t *testing.T) {
	data := strings.Split(testData, "\n")
	input := make([]int, len(data))
	for k, v := range data {
		n, _ := strconv.Atoi(v)
		input[k] = n
	}
	if part1(input, 5) != 127 {
		t.Fail()
	}
	if part2(input, 127) != 62 {
		t.Fail()
	}

}
