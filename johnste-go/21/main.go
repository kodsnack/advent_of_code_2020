package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	run("./example-data.txt")
	run("./data.txt")
}

func run(path string) {
	foods := parse(path)

	a(foods)
}

func a(foods []string) int {

	list := map[string][]string{}
	allIngredients := []string{}
	for _, food := range foods {
		parts := strings.Split(food, " (contains")

		ingredients := []string{}
		for _, ingredient := range strings.Split(parts[0], " ") {
			ingredients = append(ingredients, strings.Trim(ingredient, " "))
			allIngredients = append(allIngredients, strings.Trim(ingredient, " "))
		}

		allergens := []string{}
		for _, allergen := range strings.Split(parts[1], ",") {
			allergens = append(allergens, strings.Trim(allergen, " )"))
		}

		for _, allergen := range allergens {
			if a, ok := list[allergen]; ok {

				list[allergen] = intersect(a, ingredients)

			} else {
				list[allergen] = ingredients
			}
		}

	}

	lastLength := len(allIngredients)
	okIngredients := allIngredients
	dangerList := []string{}
	for {

		for allergen, foods := range list {
			if len(foods) == 1 { // known
				for allergen2, foods2 := range list {
					if allergen != allergen2 {
						list[allergen2] = delete(foods2, foods[0])
					}
				}
			}
		}

		okIngredients = allIngredients
		dangerList = []string{}
		for allergen, thefoods := range list {
			if len(thefoods) == 1 { // known
				okIngredients = delete(okIngredients, thefoods[0])

				dangerList = append(dangerList, allergen+":"+thefoods[0])
			}
		}

		sort.Strings(dangerList)

		for i := 0; i < len(dangerList); i++ {
			dangerList[i] = strings.Split(dangerList[i], ":")[1]
		}

		if len(okIngredients) < lastLength {
			lastLength = len(okIngredients)
		} else {
			break
		}

	}
	fmt.Println("A&B", len(okIngredients), strings.Join(dangerList, ","))

	return 0
}

func contain(a, b []string) bool {

	for _, s := range a {
		for _, s2 := range b {
			if s == s2 {
				return true
			}
		}
	}
	return false
}

func intersect(a, b []string) []string {
	res := []string{}
	for _, s := range a {
		for _, s2 := range b {
			if s == s2 {
				res = append(res, s)
			}
		}
	}
	return res
}

func delete(a []string, target string) []string {
	res := []string{}
	for _, s := range a {
		if s != target {
			res = append(res, s)
		}
	}
	return res
}

func parse(path string) []string {
	data, err := ioutil.ReadFile(path)
	check(err)

	content := strings.Split(string(data), "\n")
	// for i, row := range content {

	// }

	return content
}

// "a" [["a" "a"]["a b"] 3] 5
