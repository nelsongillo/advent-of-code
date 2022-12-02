package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

// Opponent:		Me:
//
//	A -> Rock			X -> Rock
//	B -> Paper			Y -> Paper
//	C -> Scissors		Z -> Scissors
type round []string

func readFile(fpath string) []round {
	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output []round
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()
		output = append(output, strings.Split(line, " "))
	}

	return output
}

func scoreWin(r round) int {
	winning := map[string]string{
		"A": "Y",
		"B": "Z",
		"C": "X",
	}

	draw := map[string]string{
		"A": "X",
		"B": "Y",
		"C": "Z",
	}

	if me, ok := winning[r[0]]; ok {
		if strings.EqualFold(me, r[1]) {
			return 6
		}
	}

	if me, ok := draw[r[0]]; ok {
		if strings.EqualFold(me, r[1]) {
			return 3
		}
	}

	return 0
}

func scoreChoice(r round) int {
	switch r[1] {
	case "X":
		return 1
	case "Y":
		return 2
	case "Z":
		return 3
	default:
		return 0
	}
}

func chooseGuide(r round) {
	switch r[1] {
	case "X": // Loose
		switch r[0] {
		case "A":
			r[1] = "Z"
		case "B":
			r[1] = "X"
		case "C":
			r[1] = "Y"
		default:
			return
		}
	case "Y": // Draw
		switch r[0] {
		case "A":
			r[1] = "X"
		case "B":
			r[1] = "Y"
		case "C":
			r[1] = "Z"
		default:
			return
		}
	case "Z": // Win
		switch r[0] {
		case "A":
			r[1] = "Y"
		case "B":
			r[1] = "Z"
		case "C":
			r[1] = "X"
		default:
			return
		}
	default:
		return
	}
}

func calcScore(r round) int {
	return scoreWin(r) + scoreChoice(r)
}

func solveOne(rounds []round) int {
	total := 0
	for _, r := range rounds {
		total += calcScore(r)
	}

	return total
}

func solveTwo(rounds []round) int {
	total := 0
	for _, r := range rounds {
		chooseGuide(r)
		total += calcScore(r)
	}

	return total
}

func main() {
	vals := readFile("data.txt")

	fmt.Printf("Solution 1: %d\n", solveOne(vals))
	fmt.Printf("Solution 2: %d\n", solveTwo(vals))
}
