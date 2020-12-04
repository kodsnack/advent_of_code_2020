package main

import (
	"strings"
	"testing"
)

const testData = `..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#`

func Test_Ex1(t *testing.T) {
	data := strings.Split(testData, "\n")
	s := slope{mapData: data}
	x, trees := 0, 0
	for y, _ := range s.mapData {
		if s.hasTree(y, x) {
			trees++
		}
		x += 3
	}
	if trees != 7 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	data := strings.Split(testData, "\n")
	s := slope{mapData: data}
	product := s.travel(1, 1) * s.travel(1, 3) * s.travel(1, 5) * s.travel(1, 7) * s.travel(2, 1)
	if product != 336 {
		t.Fail()
	}
}
