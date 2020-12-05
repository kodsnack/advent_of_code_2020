```
CL-USER> (ql:quickload "advent-of-code")
To load "advent-of-code":
  Load 1 ASDF system:
    advent-of-code
; Loading "advent-of-code"
..................................................
[package aoc].....................................
[package aoc2020.day01]...........................
[package aoc2020.day02]...........................
[package aoc2020.day03]...........................
[package aoc2020.day04]...........................
[package aoc2020.tests].....
("advent-of-code")
CL-USER> (fiasco:all-tests)
ALL-TESTS (Suite)
  AOC2020.TESTS (Suite)
    DAY01.................................................................[ OK ]
    DAY02.................................................................[ OK ]
    DAY03.................................................................[ OK ]
    DAY04.................................................................[ OK ]

T
(#<test-run of ALL-TESTS: 6 tests, 8 assertions, 0 failures in 0.141 sec>)
CL-USER>
```
