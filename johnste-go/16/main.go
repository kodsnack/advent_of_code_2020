package main

import (
	"fmt"
	"io/ioutil"
	"regexp"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type span struct {
	min, max int
}

type field struct {
	name  string
	spans []span
}

func main() {
	//runAssignment("./example-data.txt")
	runAssignment("./example-data-2.txt")
	runAssignment("./data.txt")
}

func runAssignment(path string) {
	fields, myTicket, tickets := parse(path)
	errorRate, validTickets := a(fields, myTicket, tickets)
	departureValue := b(fields, myTicket, validTickets)
	fmt.Println(path)
	fmt.Println("A", errorRate)
	fmt.Println("B", departureValue)
	fmt.Println("----")
}

func a(fields map[string][]span, myTicket []int, tickets [][]int) (int, [][]int) {
	allSpans := []span{}
	for _, field := range fields {
		allSpans = append(allSpans, field...)
	}

	invalidNumbers := []int{}
	validTickets := [][]int{}

	for _, ticket := range tickets {
		validTicket := true

		for _, field := range ticket {
			valid := false

			for _, span := range allSpans {
				if field >= span.min && field <= span.max {
					valid = true
				}
			}

			if !valid {
				invalidNumbers = append(invalidNumbers, field)
				validTicket = false
			}
		}

		if validTicket {
			validTickets = append(validTickets, ticket)
		}
	}

	sum := 0
	for _, number := range invalidNumbers {
		sum += number
	}

	return sum, validTickets
}

func b(fields map[string][]span, myTicket []int, tickets [][]int) int {

	validIndices := map[string][]int{}

	for fieldName, spans := range fields {
		for i := 0; i < len(tickets[0]); i++ {
			values := []int{}

			for _, ticket := range tickets {
				values = append(values, ticket[i])
			}

			if allValid(fieldName, values, spans) {
				if val, ok := validIndices[fieldName]; ok {
					validIndices[fieldName] = append(val, i)
				} else {
					validIndices[fieldName] = []int{i}
				}
			}

		}
	}

	for {
		multiples := false
		for fieldName, candidates := range validIndices {
			//			fmt.Println(fieldName, candidates)

			if len(candidates) == 1 {
				candidate := candidates[0]
				//fmt.Println("Remove", fieldName, ":", candidate)
				for fieldNameInner, candidates := range validIndices {
					if fieldName == fieldNameInner {
						continue
					}
					newValues := removeItem(candidate, candidates)
					//fmt.Println("Removing", candidate, newValues, "for", fieldNameInner)
					validIndices[fieldNameInner] = newValues

				}
			} else {
				multiples = true
			}
		}

		if !multiples {
			break
		}
	}

	// b, _ := json.MarshalIndent(validIndices, "", "  ")
	// fmt.Println(string(b))

	product := 1

	for name, index := range validIndices {
		if strings.HasPrefix(name, "departure") {
			product *= myTicket[index[0]]
		}
	}

	return product
}

func removeItem(target int, values []int) []int {
	tmp := []int{}
	for _, value := range values {
		if target == value {
			continue
		}

		tmp = append(tmp, value)
	}
	return tmp
}

func allValid(fn string, values []int, spans []span) bool {

	allValid := true
	for _, value := range values {
		atLeastOneValid := false
		for _, span := range spans {
			if value >= span.min && value <= span.max {
				atLeastOneValid = true
			}
		}

		if !atLeastOneValid {
			allValid = false
		}
	}

	return allValid
}

func parse(path string) (map[string][]span, []int, [][]int) {
	content, err := ioutil.ReadFile(path)
	check(err)

	data := strings.Split(string(content), "\n")

	// Get fields
	reField := regexp.MustCompile("^([\\w ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$")
	fields := map[string][]span{}
	for {
		row := data[0]
		data = data[1:]

		if row == "your ticket:" {
			break
		}

		matches := reField.FindStringSubmatch(row)
		if len(matches) > 0 {
			numbers := toInts(matches[1:])
			spans := []span{{numbers[1], numbers[2]}, {numbers[3], numbers[4]}}
			fields[matches[1]] = spans
		}

	}

	// Get my ticket
	myTicket := toInts(strings.Split(data[0], ","))
	data = data[1:]

	// Eat data
	for {
		row := data[0]
		data = data[1:]
		if row == "nearby tickets:" {
			break
		}
	}

	// Get other tickets
	tickets := [][]int{}
	for len(data) > 0 {
		ticket := toInts(strings.Split(data[0], ","))
		tickets = append(tickets, ticket)
		data = data[1:]
	}
	return fields, myTicket, tickets

}

func toInts(list []string) []int {
	result := []int{}
	for _, val := range list {
		number, _ := strconv.Atoi(val)
		result = append(result, number)
	}
	return result
}
