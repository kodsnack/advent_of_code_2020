package main

import (
	"fmt"
	"io/ioutil"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	runAssignment("./example-data.txt")

	runAssignment("./data.txt")
}

func runAssignment(path string) {
	d := parse(path)

	cubes := a(d)
	fmt.Println("A", cubes)
}

func a(d dimension) int {

	print(0, d)

	for i := 0; i < 6; i++ {

		d2 := dimension{}

		for pos, _ := range d {
			for x := -1; x < 2; x++ {
				for y := -1; y < 2; y++ {
					for z := -1; z < 2; z++ {
						if x == 0 && y == 0 && z == 0 {
							continue
						}

						pos2 := vec3d{
							pos.x + x,
							pos.y + y,
							pos.z + z,
						}

						neighbours := getNeighbours(d, pos2)

						point := d[pos2]

						if point && (neighbours >= 2 && neighbours <= 3) {

							d2[pos2] = true
						} else if !point && neighbours == 3 {

							d2[pos2] = true
						}
					}
				}
			}
		}

		fmt.Printf("After %d cycles:\n", i+1)
		print(0, d2)
		d = d2
	}

	return len(d)
}

func print(z int, d dimension) {

	minY, minX := 10000, 10000
	maxY, maxX := -1000, -10000

	for pos, _ := range d {
		if pos.x < minX {
			minX = pos.x
		}
		if pos.x > maxX {
			maxX = pos.x
		}
		if pos.y < minY {
			minY = pos.y
		}
		if pos.y > maxY {
			maxY = pos.y
		}
	}

	for y := minY - 1; y < maxY+2; y++ {
		for x := minX - 1; x < maxX+2; x++ {
			if d[vec3d{x, y, z}] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Print("\n")
	}
}

func getNeighbours(d dimension, pos vec3d) int {
	sum := 0

	for x := -1; x < 2; x++ {
		for y := -1; y < 2; y++ {
			for z := -1; z < 2; z++ {
				if x == 0 && y == 0 && z == 0 {
					continue
				}

				pos2 := vec3d{
					pos.x + x,
					pos.y + y,
					pos.z + z,
				}

				if d[pos2] {
					sum += 1
				}

			}
		}
	}

	return sum

}

type vec3d struct {
	x, y, z int
}

type dimension = map[vec3d]bool

func parse(path string) dimension {
	content, err := ioutil.ReadFile(path)
	check(err)

	data := strings.Split(string(content), "\n")

	dimension := dimension{}
	for x := 0; x < len(data); x++ {
		for y := 0; y < len(data[x]); y++ {
			if data[y][x] == '#' {
				pos := vec3d{x, y, 0}
				dimension[pos] = true
			}

		}
	}

	return dimension
}
