package main

import (
	"strings"
	"testing"
)

const testData = `nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6`

func Test_Ex1(t *testing.T) {
	p := parseInput(strings.Split(testData, "\n"))
	if tot, _ := p.run(); tot != 5 {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	p := parseInput(strings.Split(testData, "\n"))
	if p.fixProgram() != 8 {
		t.Fail()
	}
}
