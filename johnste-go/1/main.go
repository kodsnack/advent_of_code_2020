package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
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

	numbers := []int{}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())

		number, err := strconv.Atoi(scanner.Text())

		check(err)
		numbers = append(numbers, number)
	}

	fmt.Println(len(numbers))

	if err := scanner.Err(); err != nil {
		log.Fatal(err)

	}

	// O(n³) lol
	for _, value := range numbers {
		for _, value2 := range numbers {
			for _, value3 := range numbers {
				if value+value2+value3 == 2020 {
					fmt.Printf("Answer: %d\n", value*value2*value3)
				}
			}
		}
	}
}
