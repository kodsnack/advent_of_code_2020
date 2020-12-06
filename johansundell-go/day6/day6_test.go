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
	if countYes(input) != 11 {
		t.Fail()
	}
}
