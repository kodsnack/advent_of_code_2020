package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	//run("./example-data2.txt")
	run("./data.txt")
}

func run(path string) {
	rule, messages := parse(path)

	result := a(rule, messages)
	fmt.Println(path, "A", result)
	// result = b(d)
	// fmt.Println(path, "B", result)
}

func a(rule string, messages []string) int {

	re := regexp.MustCompile(rule)

	fmt.Println(rule)
	sum := 0
	for _, message := range messages {

		if re.MatchString(message) {
			fmt.Println(message)
			sum += 1

		}
	}
	return sum
}

type dictionary = map[int]string

func parse(path string) (string, []string) {
	data, err := ioutil.ReadFile(path)
	check(err)

	dict := dictionary{}
	messages := []string{}
	therule := ""

	r := regexp.MustCompile("\\\"(\\w)\\\"")

	//depth := 0
	content := strings.Split(string(data), "\n")
	for i, row := range content {
		if row == "" {
			messages = content[i+1:]
			break
		}

		parts := strings.Split(row, ":")
		id, _ := strconv.Atoi(parts[0])
		pattern := strings.Trim(parts[1], " ")

		matches := r.FindStringSubmatch(pattern)
		if len(matches) > 0 {
			letter := matches[1]
			dict[id] = letter
			//therule = therule + "(" + letter + ")"
			continue
		}

		//therule = therule + "(" + pattern + ")"
		dict[id] = "(" + pattern + ")"
	}

	therule = dict[0]
	depth := 0
	replacex := func(input []byte) []byte {
		id, _ := strconv.Atoi(string(input))

		if id == 8 {
			return []byte("(42)+")
		} else if id == 11 {
			str := "("
			for i := 1; i < 8; i++ {
				str += "(" + strings.Repeat("(42)", i) + strings.Repeat("(31)", i) + ")"
				if i < 7 {
					str += "|"
				}
			}
			str += ")"

			return []byte(str)
		}

		return []byte(dict[id])
	}

	for {

		re := regexp.MustCompile("\\d+")

		if !re.MatchString(therule) {
			break
		}
		depth += 1
		therule = string(re.ReplaceAllFunc([]byte(therule), replacex))
		//fmt.Println(therule)
	}

	therule = "^" + strings.ReplaceAll(therule, " ", "") + "$"

	return therule, messages
}

// "a" [["a" "a"]["a b"] 3] 5
