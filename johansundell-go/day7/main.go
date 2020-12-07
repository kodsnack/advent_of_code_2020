package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/johansundell/advent_of_code_2017/johansundell-go/adventofcode2017"
)

func main() {
	data, _ := adventofcode2017.GetInput("day7.txt")
	rules := getRules(strings.Split(data, "\n"))
	fmt.Println(part1(rules))
	fmt.Println(part2(rules, make(map[string]int), "shiny gold"))
}

func getRules(input []string) map[string]map[string]int {
	rules := make(map[string]map[string]int)
	for _, v := range input {
		strs := strings.Split(v, " contain ")
		mainBag := strs[0][:len(strs[0])-5]
		rules[mainBag] = make(map[string]int)
		for _, bag := range strings.Split(strs[1][:len(strs[1])-1], ", ") {
			parts := strings.SplitN(bag, " ", 2)
			n, _ := strconv.Atoi(parts[0])
			offset := 4
			if strings.Contains(parts[1], "bags") {
				offset = 5
			}
			rules[mainBag][parts[1][:len(parts[1])-offset]] = n
		}
	}
	return rules
}

func part1(rules map[string]map[string]int) int {
	isFound := true
	list := make(map[string]bool)
	list["shiny gold"] = true
	for isFound {
		isFound = false
		for k, v := range rules {
			if list[k] {
				continue
			}
			for key, _ := range v {
				if list[key] {
					list[k] = true
					isFound = true
					break
				}
			}
		}
	}
	return len(list) - 1
}

func part2(rules map[string]map[string]int, list map[string]int, name string) int {
	if val, ok := list[name]; ok {
		return val
	}
	count := 0
	for k, v := range rules[name] {
		count += v * (1 + part2(rules, list, k))
	}
	list[name] = count
	return count
}
