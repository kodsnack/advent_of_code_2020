package main

import (
	"fmt"
	"io/ioutil"
	"math"
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
	//run("./example-data.txt")
	run("./data.txt")
}

func run(path string) {
	squares := parse(path)
	result := a(squares)
	fmt.Println(path, "A", (result))
	result = b(squares)
	fmt.Println(path, "B", result)
}

func a(squares map[int]square) int {

	matches := getMatches(squares)
	//fmt.Println(matches)

	product := 1
	for id, match := range matches {
		if match == 2 {
			product *= id
		}
	}
	return product
}

type vec2d struct {
	x, y int
}

func b(squares map[int]square) int {

	matches := getMatches(squares)
	//fmt.Println(matches)

	startSquare := square{}
	for id, match := range matches {
		if match == 2 {
			startSquare = squares[id]
		}
	}

	puzzle := puzzleType{}

	puzzle[vec2d{2, 2}] = startSquare

	delete(squares, startSquare.id)

	for len(squares) > 0 {
		for pos, s := range puzzle {
			//fmt.Println("  Checking", pos, s.id, len(puzzle), len(squares))
			for _, square := range squares {
				if square.id == s.id {
					continue
				}

				if match, _, s2, direction := s.match(square); match {
					var newpos vec2d

					switch direction {
					case North:
						newpos = vec2d{pos.x, pos.y - 1}
					case East:
						newpos = vec2d{pos.x + 1, pos.y}
					case South:
						newpos = vec2d{pos.x, pos.y + 1}
					case West:
						newpos = vec2d{pos.x - 1, pos.y}
					}

					puzzle[newpos] = s2

					//fmt.Println("    Add", s2.id, "to the", direction, "of", s.id, newpos)
					delete(squares, s2.id)
					break
				}
			}
		}
	}

	puzzle = cleanup(puzzle)

	width := int(math.Sqrt(float64(len(puzzle))))
	height := 8

	bigass := make([]string, width*height)

	//fmt.Println(bigass)
	for y := 0; y < width; y++ {

		for y2 := 0; y2 < height; y2++ {

			str := ""
			for x := 0; x < width; x++ {
				pos := vec2d{x, y}
				str += puzzle[pos].pixels[y2]
			}

			bigass[y*height+y2] = str
		}

	}

	//fmt.Println(strings.Join(bigass, "\n"))

	bigpuzzle := square{420, bigass}
	gap := height*width - 19
	fmt.Println("gap", gap)
	gapstr := fmt.Sprintf("%d", gap)

	monster := "(#)(?:[#.\\s]){" + gapstr + "}(#)....(#)(#)....(#)(#)....(#)(#)(#)(?:[#.\\s]){" + gapstr + "}(#)..(#)..(#)..(#)..(#)..(#)"
	re := regexp.MustCompile(monster)

	foundMatch := false

	monstermatches := []string{}

	for i := 0; i < 4; i++ {
		monstermatches = re.FindAllString(strings.Join(bigpuzzle.pixels, ""), -1)
		if len(monstermatches) > 0 {
			fmt.Println(bigpuzzle)
			fmt.Println(len(bigpuzzle.pixels), len(bigpuzzle.pixels[0]))
			fmt.Println("match", len(monstermatches))
			foundMatch = true
			break
		}
		bigpuzzle = bigpuzzle.rotate()
	}

	if !foundMatch {
		bigpuzzle = bigpuzzle.flipX()
		for i := 0; i < 4; i++ {
			monstermatches = re.FindAllString(strings.Join(bigpuzzle.pixels, ""), -1)
			if len(monstermatches) > 0 {
				fmt.Println(bigpuzzle)
				fmt.Println(len(bigpuzzle.pixels), len(bigpuzzle.pixels[0]))
				fmt.Println("match", len(monstermatches))
				break
			}
			bigpuzzle = bigpuzzle.rotate()
		}
	}

	finalpuzzle := strings.Join(bigpuzzle.pixels, "\n")
	gap += 1 //account for newlines
	gapstr = fmt.Sprintf("%d", gap)
	monster = "(#)(?:[#.\\s]){" + gapstr + "}(#)....(#)(#)....(#)(#)....(#)(#)(#)(?:[#.\\s]){" + gapstr + "}(#)..(#)..(#)..(#)..(#)..(#)"
	re = regexp.MustCompile(monster)

	totes := 0
	for {
		//replacematches2 := re.FindStringSubmatch(finalpuzzle)
		replacematches := re.FindStringSubmatchIndex(finalpuzzle)
		// fmt.Println("va", replacematches2)
		// fmt.Println("vx", replacematches)
		for i := 2; i < len(replacematches); i += 2 {
			//fmt.Println("reppp", finalpuzzle, replacematches[i], replacematches[i]+1, finalpuzzle[:replacematches[i]]+"O"+finalpuzzle[:replacematches[i]])

			finalpuzzle = finalpuzzle[:replacematches[i]] + "O" + finalpuzzle[replacematches[i]+1:]
		}

		totes += 1
		monstermatches = re.FindAllString(finalpuzzle, -1)
		if len(monstermatches) == 0 {
			break
		}

	}

	rehash := regexp.MustCompile("#")
	matches2 := rehash.FindAllStringIndex(finalpuzzle, -1)

	fmt.Println(finalpuzzle)
	fmt.Println("angelina", totes, len(matches2))

	return 0
}

type puzzleType = map[vec2d]square

func cleanup(puzzle puzzleType) puzzleType {
	min := vec2d{1000, 1000}

	for pos, _ := range puzzle {
		if pos.x < min.x {
			min.x = pos.x
		}
		if pos.y < min.y {
			min.y = pos.y
		}
	}

	translatedPuzzle := puzzleType{}

	for pos, square := range puzzle {
		square.pixels = square.pixels[1 : len(square.pixels)-1]

		for i := 0; i < len(square.pixels); i++ {
			square.pixels[i] = square.pixels[i][1 : len(square.pixels[i])-1]
		}

		translatedPuzzle[vec2d{pos.x - min.x, pos.y - min.y}] = square
	}

	return translatedPuzzle
}

func getMatches(squares map[int]square) map[int]int {
	matches := map[int]int{}
	for _, s := range squares {
		for _, s2 := range squares {
			if s.id == s2.id {
				continue
			}

			if match, _, _, _ := s.match(s2); match {
				if _, ok := matches[s.id]; ok {
					matches[s.id]++
				} else {
					matches[s.id] = 1
				}
			}
		}
	}
	return matches
}

func parse(path string) map[int]square {
	data, err := ioutil.ReadFile(path)
	check(err)

	squaresmap := map[int]square{}
	for _, lines := range strings.Split(string(data), "\n\n") {
		if len(strings.Split(lines, "\n")[0]) < 9 {
			continue
		}
		id, _ := strconv.Atoi(strings.Split(lines, "\n")[0][5:9])
		pixels := strings.Split(lines, "\n")[1:]
		square := square{id, pixels}

		squaresmap[id] = square
	}

	return squaresmap
}

//22 monsters, 2278 too high
