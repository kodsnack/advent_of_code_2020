package main

import (
	"bufio"
	"fmt"
	"os"
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

func main() {

	data := readFile("./data.txt")

	for {
		toFlip := flipA(&data, 4, getAdjacent)
		if toFlip == 0 {
			break
		}
	}

	allSeats := ""
	for _, row := range data {
		allSeats += row
	}

	fmt.Println("A", countOccupied(allSeats))

	data = readFile("./data.txt")
	for {
		toFlip := flipA(&data, 5, getAdjacentDirections)
		if toFlip == 0 {
			break
		}
	}

	allSeats = ""
	for _, row := range data {
		allSeats += row
	}

	fmt.Println("B", countOccupied(allSeats))
}

type node struct {
	x, y int
}

func flipA(seats *[]string, neighbourMax int, callback func(seats *[]string, x, y int, row string) string) int {

	toFlip := []node{}

	for y, row := range *seats {
		for x, _ := range row {
			adjacent := callback(seats, x, y, row)
			occupied := countOccupied(adjacent)
			if isUnoccupied((*seats)[y][x]) && occupied == 0 && !isFloor((*seats)[y][x]) {
				toFlip = append(toFlip, node{x, y})
			} else if isOccupied((*seats)[y][x]) && occupied >= neighbourMax {
				toFlip = append(toFlip, node{x, y})
			}
		}
	}

	for _, node := range toFlip {
		row := (*seats)[node.y]
		if isOccupied(row[node.x]) {
			(*seats)[node.y] = row[:node.x] + "L" + row[node.x+1:]
		} else if isUnoccupied(row[node.x]) {
			(*seats)[node.y] = row[:node.x] + "#" + row[node.x+1:]
		}
	}

	return len(toFlip)
}

func isUnoccupied(seat byte) bool {
	return seat == 'L' || seat == '.'
}

func isOccupied(seat byte) bool {
	return seat == '#'
}

func isFloor(seat byte) bool {
	return seat == '.'
}

func countOccupied(seats string) int {
	sum := 0
	for _, seat := range seats {
		if isOccupied(byte(seat)) {
			sum += 1
		}
	}
	return sum
}

func getAdjacent(seats *[]string, x, y int, row string) string {
	adjacent := ""

	for y2 := y - 1; y2 <= y+1; y2++ {
		for x2 := x - 1; x2 <= x+1; x2++ {
			if y2 < 0 || x2 < 0 || y2 >= len(*seats) || x2 >= len(row) {
				continue
			}

			if y2 == y && x2 == x {
				continue
			}

			adjacent += string((*seats)[y2][x2])
		}
	}

	return adjacent
}

func getAdjacentDirections(seats *[]string, x, y int, row string) string {
	adjacent := ""

	directions := []node{{-1, 0}, {-1, 1}, {0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}, {-1, -1}}

	for _, direction := range directions {

		x2 := x
		y2 := y

		for {

			x2 = x2 + direction.x
			y2 = y2 + direction.y

			if y2 < 0 || x2 < 0 || y2 >= len(*seats) || x2 >= len(row) {
				break
			}

			if !isFloor((*seats)[y2][x2]) {
				adjacent += string((*seats)[y2][x2])
				break
			}
		}

	}

	return adjacent
}
