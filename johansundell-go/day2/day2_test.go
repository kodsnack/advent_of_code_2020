package main

import (
	"testing"
)

const testData = `1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc`

func Test_Ex1(t *testing.T) {
	pp := parseData(testData)
	if !pp[0].Validate() {
		t.Fail()
	}
	if pp[1].Validate() {
		t.Fail()
	}
	if !pp[2].Validate() {
		t.Fail()
	}
}

func Test_Ex2(t *testing.T) {
	pp := parseData(testData)
	if !pp[0].ValidateSecondSolution() {
		t.Fail()
	}
	if pp[1].ValidateSecondSolution() {
		t.Fail()
	}
	if pp[2].ValidateSecondSolution() {
		t.Fail()
	}
}
