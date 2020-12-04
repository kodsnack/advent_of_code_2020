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

	passport := ""
	for scanner.Scan() {
		line := scanner.Text()

		passport += " " + line

		if line == "" {
			result = append(result, passport)
			passport = ""
			continue
		}
	}

	result = append(result, passport)

	return result
}

func isTree(data []string, x int, y int, width int) bool {
	return data[y][x%width] == '#'
}

type pair struct {
	key, value string
}

func contains(field string, fields []string) bool {
	for _, v := range fields {
		if v == field {
			return true
		}
	}
	return false
}

func pairContains(field string, fields []pair) bool {
	for _, v := range fields {
		if v.key == field {
			return true
		}
	}
	return false
}

func main() {

	data := readFile("./data.txt")

	validPassports := 0

	required := []string{"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}

	// 4A
	for _, value := range data {
		pairs := strings.Fields(value)

		fields := []string{}
		for _, field := range pairs {
			fields = append(fields, strings.Split(field, ":")[0])
		}

		valid := true
		for _, value := range required {
			if !contains(value, fields) {
				valid = false
			}
		}

		fmt.Println(fields)

		fmt.Printf("%t\n", valid)

		if valid {
			validPassports += 1
		}
	}

	fmt.Printf("3A Answer: %d\n", validPassports)

	// 4B
	createYearValidator :=
		func(min, max int) func(string) bool {
			return func(year string) bool {
				if len(year) != 4 {
					return false
				}

				match, _ := regexp.MatchString("\\d{4}", year)

				if !match {
					return false
				}

				yearNum, _ := strconv.Atoi(year)

				if yearNum < min || yearNum > max {
					return false
				}

				return true
			}
		}

	validators := map[string]func(string) bool{
		"byr": createYearValidator(1920, 2002),
		"iyr": createYearValidator(2010, 2020),
		"eyr": createYearValidator(2020, 2030),
		"hgt": func(height string) bool {

			r := regexp.MustCompile("^(\\d+)(cm|in)$")
			matches := r.FindStringSubmatch(height)

			if len(matches) != 3 {
				return false
			}

			length, _ := strconv.Atoi(matches[1])
			unit := matches[2]

			if unit == "cm" && (length < 150 || length > 193) {
				return false
			} else if unit == "in" && (length < 59 || length > 76) {
				return false
			}

			return true
		},
		"hcl": func(color string) bool {
			match, _ := regexp.MatchString("^#[0-9a-f]{6}$", color)
			return match
		},
		"ecl": func(color string) bool {
			validColors := []string{"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}
			return contains(color, validColors)
		},
		"pid": func(passportId string) bool {
			match, _ := regexp.MatchString("^\\d{9}$", passportId)
			return match
		},
	}

	validPassports = 0
	for _, value := range data {
		pairs := strings.Fields(value)

		fields := []pair{}
		for _, field := range pairs {
			pair := pair{strings.Split(field, ":")[0], strings.Split(field, ":")[1]}
			fields = append(fields, pair)
		}

		valid := true
		for _, value := range required {
			if !pairContains(value, fields) {
				valid = false
			}
		}

		for _, value := range fields {
			fmt.Println(value)
			if validators[value.key] == nil {
				continue
			}
			if valid {
				valid = validators[value.key](value.value)
			}
		}

		fmt.Println(fields)
		fmt.Printf("%t\n", valid)

		if valid {
			validPassports += 1
		}
	}

	fmt.Printf("3B Answer: %d\n", validPassports)

}
