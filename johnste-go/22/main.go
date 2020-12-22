package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	run("./example.txt")
	run("./example2.txt")
	run("./data.txt")
}

func run(path string) {
	data := parse(path)
	a(data)
	data = parse(path)
	b(data)
}

func a(g game) {
	sum, winner := play(g, 0)
	fmt.Println("A", sum, winner)
}

func b(g game) {

	sum, winner := play(g, 0)
	fmt.Println("B", sum, winner)
}

func play(g game, depth int) (int, int) {
	g = deepCopy(g)

	rounds := 1
	same := map[string]bool{}

	for {

		key1 := "p1:" + strings.Join(toString(g[1]), ",")
		key2 := "p2:" + strings.Join(toString(g[2]), ",")

		forceWinner := 0
		if _, ok := same[key1+key2]; ok {
			return 0, 1
		}

		same[key1+key2] = true

		card1, rest1 := shift(g[1])
		card2, rest2 := shift(g[2])

		if forceWinner == 0 && card1 <= len(rest1) && card2 <= len(rest2) {

			g2 := deepCopy(g)
			g2[1] = rest1[:card1]
			g2[2] = rest2[:card2]
			//time.Sleep(5500 * time.Millisecond)
			_, forceWinner = play(g2, depth+1)
		}

		if forceWinner == 1 || (forceWinner != 2 && card1 > card2) {
			g[1] = append(rest1, card1, card2)
			g[2] = rest2

		} else if forceWinner == 2 || card2 > card1 {
			g[2] = append(rest2, card2, card1)
			g[1] = rest1

		}

		if len(g[1]) == 0 || len(g[2]) == 0 {
			break
		}

		rounds += 1
	}

	winner := g[1]
	winnerId := 1

	if len(g[2]) != 0 {
		winner = g[2]
		winnerId = 2
	}

	sum := 0
	var first int
	for i := len(winner); len(winner) > 0; i-- {
		first, winner = shift(winner)

		sum += i * first
	}

	return sum, winnerId
}

type game = map[int][]int

func shift(items []int) (first int, rest []int) {
	first = items[0]
	rest = items[1:]
	return
}

func deepCopy(g game) game {
	g2 := game{}

	for k, val := range g {
		g2[k] = copy(val)
	}
	return g2
}

func copy(l []int) []int {
	l2 := []int{}
	for _, v := range l {
		l2 = append(l2, v)
	}
	return l2
}

func toString(list []int) []string {
	s := []string{}
	for _, v := range list {
		s = append(s, fmt.Sprintf("%d", v))
	}
	return s
}

func parse(path string) game {
	data, err := ioutil.ReadFile(path)
	check(err)

	players := game{}
	for _, player := range strings.Split(string(data), "\n\n") {
		playerData := strings.Split(player, "\n")
		id, _ := strconv.Atoi(string(playerData[0][7]))

		cards := []int{}
		for _, card := range playerData[1:] {
			if cardNum, err := strconv.Atoi(strings.Trim(card, "\n ")); err == nil {
				cards = append(cards, cardNum)
			}
		}
		players[id] = cards
	}

	return players
}
