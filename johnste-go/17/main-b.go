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
	runAssignment("./data.txt")
}

func runAssignment(path string) {
	d := parse(path)
	cubes := b(d)
	fmt.Println("B", cubes)
}

func b(d dimension) int {

	for i := 0; i < 6; i++ {

		d2 := dimension{}

		for pos, _ := range d {
			for x := -1; x < 2; x++ {
				for y := -1; y < 2; y++ {
					for z := -1; z < 2; z++ {
						for w := -1; w < 2; w++ {
							if x == 0 && y == 0 && z == 0 && w == 0 {
								continue
							}

							pos2 := vec4d{
								pos.x + x,
								pos.y + y,
								pos.z + z,
								pos.w + w,
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
		}

		d = d2
	}

	return len(d)
}

func getNeighbours(d dimension, pos vec4d) int {
	sum := 0

	for x := -1; x < 2; x++ {
		for y := -1; y < 2; y++ {
			for z := -1; z < 2; z++ {
				for w := -1; w < 2; w++ {
					if x == 0 && y == 0 && z == 0 && w == 0 {
						continue
					}

					pos2 := vec4d{
						pos.x + x,
						pos.y + y,
						pos.z + z,
						pos.w + w,
					}

					if d[pos2] {
						sum += 1
					}
				}
			}
		}
	}

	return sum
}

type vec4d struct {
	x, y, z, w int
}

type dimension = map[vec4d]bool

func parse(path string) dimension {
	content, err := ioutil.ReadFile(path)
	check(err)

	data := strings.Split(string(content), "\n")

	dimension := dimension{}
	for x := 0; x < len(data); x++ {
		for y := 0; y < len(data[x]); y++ {
			if data[y][x] == '#' {
				pos := vec4d{x, y, 0, 0}
				dimension[pos] = true
			}

		}
	}

	return dimension
}
