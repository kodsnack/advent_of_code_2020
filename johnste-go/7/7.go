package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func readFile(path string) []string {
	file, err := os.Open(path)
	defer file.Close()
	check(err)

	result := []string{}
	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		result = append(result, line)
	}

	return result
}

type numbag struct {
	count int
	name  string
}

func main() {

	data := readFile("./data.txt")

	sum := 0

	tree := map[string][]numbag{}

	for _, value := range data {
		name, children := processLine(value)
		tree[name] = children
	}

	for key, _ := range tree {
		if canContain("shiny gold", key, tree, 0) {
			sum += 1
			fmt.Printf("%s 💎\n", key)
		}
	}
	fmt.Printf("A: %d\n", sum)

	// B

	sum = 0
	for _, value := range tree["shiny gold"] {

		fmt.Println(value)
		sum += countContains(value, tree, 0)
	}

	fmt.Printf("B: %d\n", sum)
}

func countContains(bag numbag, tree map[string][]numbag, depth int) int {

	fmt.Printf("%s > %d %s\n", strings.Repeat(" ", depth), bag.count, bag.name)
	sum := 0
	children := tree[bag.name]

	for _, child := range children {
		childCount := countContains(child, tree, depth+1)
		sum += bag.count * childCount
	}

	sum += bag.count

	fmt.Printf("%s > %d\n", strings.Repeat(" ", depth), sum)

	return sum
}

func canContain(target string, name string, tree map[string][]numbag, depth int) bool {

	children := tree[name]

	for _, child := range children {

		if child.name == target {

			//fmt.Printf("%s >%s 💎 %d\n", strings.Repeat(" ", depth), child, depth)
			return true
		}

		if canContain(target, child.name, tree, depth+1) {
			return true
		}

	}

	return false
}

func processLine(line string) (string, []numbag) {
	fields := strings.Split(line, "contain")
	name := strings.Trim(fields[0][0:len(fields[0])-5], " ")

	others := strings.Trim(fields[1], " ")
	children := []numbag{}

	if others != "no other bags." {
		children = []numbag{}
		for _, name := range strings.Split(others, ",") {

			bag := numbag{getCount(name), normalize(name)}
			children = append(children, bag)
		}
	}

	return name, children
}

func normalize(name string) string {
	re := regexp.MustCompile(`[\d\.]|bags|bag`)
	name = re.ReplaceAllString(name, "")
	name = strings.Trim(name, " ")
	return name
}

func getCount(name string) int {
	re := regexp.MustCompile(`\d+`)

	name = re.FindString(name)
	num, _ := strconv.Atoi(strings.Trim(name, " "))

	return num

}
