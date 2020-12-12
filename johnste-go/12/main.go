package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
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

	r := regexp.MustCompile("^([NSEWLRF])(\\d+)$")

	direction := "E"
	position := vec2d{0, 0}
	for _, row := range data {

		matches := r.FindStringSubmatch(row)

		if len(matches) != 3 {
			continue
		}

		length, _ := strconv.Atoi(matches[2])
		way := matches[1]

		direction, position = travelA(direction, way, length, position)

	}

	fmt.Println("A", direction, position, abs(position.x)+abs(position.y))

	direction = "E"
	position = vec2d{0, 0}
	waypoint := vec2d{10, -1}
	for _, row := range data {

		matches := r.FindStringSubmatch(row)

		if len(matches) != 3 {
			continue
		}

		length, _ := strconv.Atoi(matches[2])
		way := matches[1]

		direction, position, waypoint = travelB(direction, way, length, position, waypoint)
	}
	fmt.Println("B", direction, position, waypoint, abs(position.x)+abs(position.y))

}

func travelA(current, way string, length int, position vec2d) (string, vec2d) {
	directions := []string{"N", "E", "S", "W"}
	index, _ := indexOf(current, directions)

	switch way {
	case "L":
		amount := (length - 360) / 90
		current = directions[(index-amount)%len(directions)]
		return current, position
	case "R":
		amount := (length + 360) / 90
		current = directions[(index+amount)%len(directions)]
		return current, position
	}

	if way == "F" {
		way = current
	}

	switch way {
	case "N":
		position.y -= length
	case "E":
		position.x += length
	case "S":
		position.y += length
	case "W":
		position.x -= length
	}

	return current, position
}

func travelB(current, way string, length int, position, waypoint vec2d) (string, vec2d, vec2d) {

	newWaypoint := waypoint
	switch way {
	case "L":
		for i := 0; i < length/90; i++ {
			newWaypoint = vec2d{newWaypoint.y, -newWaypoint.x}
		}
		return current, position, newWaypoint
	case "R":
		for i := 0; i < length/90; i++ {
			newWaypoint = vec2d{-newWaypoint.y, newWaypoint.x}
		}

		return current, position, newWaypoint
	}

	if way == "F" {
		position.y += length * waypoint.y
		position.x += length * waypoint.x
		return current, position, waypoint
	}

	switch way {
	case "N":
		waypoint.y -= length
	case "E":
		waypoint.x += length
	case "S":
		waypoint.y += length
	case "W":
		waypoint.x -= length
	}

	return current, position, waypoint
}

func indexOf(element string, data []string) (int, bool) {
	for k, v := range data {
		if element == string(v) {
			return k, true
		}
	}

	return -1, false
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
