package main

import (
	"testing"
)

func Test_Ex1(t *testing.T) {
	str := "FBFBBFFRLR"
	s := newSeat(str)
	if s.row != 44 || s.column != 5 {
		t.Fail()
	}
	if s.getId() != 357 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	str := "BFFFBBFRRR"
	s := newSeat(str)
	if s.row != 70 || s.column != 7 {
		t.Fail()
	}
	if s.getId() != 567 {
		t.Fail()
	}
}

func Test_Ex3(t *testing.T) {
	str := "FFFBBBFRRR"
	s := newSeat(str)
	if s.row != 14 || s.column != 7 {
		t.Fail()
	}
	if s.getId() != 119 {
		t.Fail()
	}
}

func Test_Ex4(t *testing.T) {
	str := "BBFFBBFRLL"
	s := newSeat(str)
	if s.row != 102 || s.column != 4 {
		t.Fail()
	}
	if s.getId() != 820 {
		t.Fail()
	}
}
