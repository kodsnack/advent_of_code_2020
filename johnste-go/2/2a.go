package main

import (
	"bufio"
	"fmt"
	"log"
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

func main() {	
	file, err := os.Open("./data.txt")
	check(err)

	defer file.Close()

	acceptable := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())

		fields := strings.Fields(scanner.Text())

		limits := strings.Split(fields[0], "-")
		min, err := strconv.Atoi(limits[0])
		check(err)
		max, err := strconv.Atoi(limits[1])
		check(err)
		character := strings.Split(fields[1], ":")[0]

		r := regexp.MustCompile(character)
		matches := len(r.FindAllStringIndex(fields[2], -1))

		if matches >= min && matches <= max {
			acceptable = acceptable + 1
		}

	}

	fmt.Printf("Answer: %d\n", acceptable)
	if err := scanner.Err(); err != nil {
		log.Fatal(err)

	}
}
