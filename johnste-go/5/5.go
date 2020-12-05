package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
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

func seatId(row, col int) int {
	return row*8 + col
}

func getRange(dir string, min, max int) (int, int) {
	half := (max - min + 1) / 2
	if dir == "F" || dir == "L" {
		return min, max - half
	} else if dir == "B" || dir == "R" {
		return min + half, max
	}
	return 0, 0
}

func getSeatID(seat string) int {
	rows := seat[0:7]
	cols := seat[7:10]

	minRow := 0
	maxRow := 127
	for _, row := range rows {
		minRow, maxRow = getRange(string(row), minRow, maxRow)
	}

	minCol := 0
	maxCol := 7
	for _, col := range cols {
		minCol, maxCol = getRange(string(col), minCol, maxCol)
	}

	if minRow != maxRow {
		panic("minRow != maxRow")
	}

	if minCol != maxCol {
		panic("minCol != maxCol")
	}

	return seatId(minRow, minCol)
}

func main() {

	data := readFile("./data.txt")
	highestSeatID := 0
	seats := []int{}

	// A
	for _, value := range data {

		seatId := getSeatID(value)
		if seatId > highestSeatID {
			highestSeatID = seatId
		}
		seats = append(seats, seatId)
	}

	fmt.Printf("A: %d\n", highestSeatID)

	// B
	sort.Ints(seats)

	lastSeat := seats[0] - 1
	for _, seat := range seats {
		if seat-lastSeat > 1 {
			fmt.Printf("B: %d\n", seat-1)
		}
		lastSeat = seat
	}
}
