package main

import (
	"strings"
	"testing"
)

const testData = `abc

a
b
c

ab
ac

a
a
a
a

b`

func Test_Ex1(t *testing.T) {
	input := strings.Split(testData, "\n\n")
	result, _ := countYes(input)
	if result != 11 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	input := strings.Split(testData, "\n\n")
	_, result := countYes(input)
	if result != 6 {
		t.Fail()
	}
}
