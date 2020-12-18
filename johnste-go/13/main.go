package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
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

type vec2d struct {
	x, y int
}

func main() {

	data := readFile("./data.txt")

	getShortestWaitBus(data)

	getSubsequent(data)
}

func getShortestWaitBus(data []string) {
	earliestDeparture, _ := strconv.Atoi((data[0]))

	for _, row := range data[1:] {

		shortestWait := math.MaxInt32
		shortestWaitBusId := 0
		buses := []int{}

		for _, bus := range strings.Split(row, ",") {
			busId, err := strconv.Atoi(bus)
			if err == nil {
				buses = append(buses, busId)
				wait := busId - earliestDeparture%busId
				if shortestWait > wait {
					shortestWait = wait
					shortestWaitBusId = busId
				}

			}
		}

		fmt.Println("A", shortestWait, shortestWaitBusId, shortestWait*shortestWaitBusId)
	}

}

type busOffset struct {
	busId, offset int
}

func getSubsequent(data []string) {

	for _, row := range data[1:] {

		buses := []busOffset{}

		for i, bus := range strings.Split(row, ",") {
			busId, err := strconv.Atoi(bus)
			if err == nil {
				buses = append(buses, busOffset{busId, i})
			}
		}

		time := 0
		increment := 1
		done := false
		usedBusIndex := -1

		for !done {

			time += increment
			done = true

			for index, bus := range buses {
				if (time+bus.offset)%bus.busId != 0 {
					done = false
					break
				}

				if usedBusIndex < index {
					increment = bus.busId * increment
					usedBusIndex = index
				}

			}

		}

		fmt.Println("B", time)
	}

}

func contains(field int, fields []int) bool {
	for _, v := range fields {
		if v == field {
			return true
		}
	}
	return false
}
