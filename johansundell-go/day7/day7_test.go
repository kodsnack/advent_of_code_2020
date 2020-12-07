package main

import (
	"strings"
	"testing"
)

const testData = `light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.`

const testData2 = `shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.`

func Test_Ex1(t *testing.T) {
	rules := getRules(strings.Split(testData, "\n"))
	if part1(rules) != 4 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	rules := getRules(strings.Split(testData, "\n"))
	if part2(rules, make(map[string]int), "shiny gold") != 32 {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	rules := getRules(strings.Split(testData2, "\n"))
	if part2(rules, make(map[string]int), "shiny gold") != 126 {
		t.Fail()
	}
}
