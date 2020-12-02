package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
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

		password := fields[2]
		limits := strings.Split(fields[0], "-")

		min, err := strconv.Atoi(limits[0])
		check(err)
		max, err := strconv.Atoi(limits[1])
		check(err)
		character := strings.Split(fields[1], ":")[0]

		matchMin := character == string(password[min-1])
		matchMax := character == string(password[max-1])

		fmt.Printf("%s : %d: %s = %t, %d %s = %t\n", password, min, string(password[min-1]), matchMin, max, string(password[max-1]), matchMax)
		if matchMin != matchMax {
			acceptable = acceptable + 1
		}

	}

	fmt.Printf("Answer: %d\n", acceptable)
	if err := scanner.Err(); err != nil {
		log.Fatal(err)

	}

}
