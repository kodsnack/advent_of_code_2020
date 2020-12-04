package main

import (
	"fmt"
	"strings"
	"testing"
)

const testData = `ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in`

func Test_Ex1(t *testing.T) {
	data := strings.Split(testData, "\n\n")
	pp := make([]passport, 0)
	for _, v := range data {
		p := newPassport(v)
		if p.isValid() {
			//fmt.Println("valid")
			if p.isValidPart2() {
				fmt.Println("valid part2")
			}
		}

		pp = append(pp, p)
	}
	t.Fail()

}
