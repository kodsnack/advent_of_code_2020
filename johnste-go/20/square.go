package main

import (
	"fmt"
	"strings"
)

type square struct {
	id     int
	pixels []string
}

func (s square) flipY() square {
	rows := []string{}
	for i := len(s.pixels) - 1; i >= 0; i-- {
		rows = append(rows, s.pixels[i])
	}
	return square{s.id, rows}
}

func (s square) flipX() square {
	rows := []string{}
	for _, row := range s.pixels {
		rows = append(rows, reverse(row))
	}
	return square{s.id, rows}
}

func (s square) rotate() square {
	rows := make([]string, len(s.pixels))

	for x := 0; x < len(s.pixels); x++ {
		str := ""
		for y := 0; y < len(s.pixels); y++ {
			str += string(s.pixels[y][x])
		}
		rows[x] = str
	}

	return (square{s.id, rows}).flipX()
}

func (s square) rotateLeft() square {
	return s.rotate().rotate().rotate()
}

func (s square) match(s2 square) (bool, square, square, Direction) {

	compare := func(s, s2 square) bool { return s2.pixels[0] == s.pixels[len(s.pixels)-1] }

	mys := s
	for j := 0; j < 4; j++ {
		for i := 0; i < 4; i++ {
			if compare(mys, s2) {
				for k := 0; k < j; k++ {
					s2 = s2.rotateLeft()
				}
				return true, s, s2, Direction(j)
			}
			s2 = s2.rotate()
		}

		s2 = s2.flipX()
		for i := 0; i < 4; i++ {
			if compare(mys, s2) {
				for k := 0; k < j; k++ {
					s2 = s2.rotateLeft()
				}
				return true, s, s2, Direction(j)
			}
			s2 = s2.rotate()
		}

		mys = mys.rotate()
	}

	var empty square
	return false, empty, empty, 0
}

func reverse(s string) string {
	result := ""
	for _, v := range s {
		result = string(v) + result
	}
	return result
}

func (s square) String() string {
	str := "Tile " + fmt.Sprintf("%v", s.id) + "\n"
	str += strings.Join(s.pixels, "\n")
	return str + "\n" + "\n"
}

type Direction int

const (
	South Direction = 0 + iota
	East
	North
	West
)

func (d Direction) String() string {
	return [...]string{"South", "East", "North", "West"}[d]
}
