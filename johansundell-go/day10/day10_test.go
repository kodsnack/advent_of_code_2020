package main

import (
	"fmt"
	"sort"
	"strconv"
	"strings"
	"testing"
)

func Test_Ex1(t *testing.T) {
	strs := strings.Split(testData, "\n")
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
	fmt.Println(no1, no3)
}

const testData = `16
10
15
5
1
11
7
19
6
12
4`

const testData2 = `28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3`
