package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type pair struct {
	left  assignment
	right assignment
}

type assignment struct {
	from int
	to   int
}

func readFile(fpath string) []pair {
	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output []pair
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()

		p := strings.Split(line, ",")
		l := intsFromStrings(strings.Split(p[0], "-"))
		r := intsFromStrings(strings.Split(p[1], "-"))

		output = append(output, pair{left: assignment{from: l[0], to: l[1]}, right: assignment{from: r[0], to: r[1]}})
	}

	return output
}

func intsFromStrings(strs []string) []int {
	var output []int

	for _, s := range strs {
		i, _ := strconv.Atoi(s)
		output = append(output, i)
	}

	return output
}

func overlap(a, b assignment) bool {
	for i := a.from; i <= a.to; i++ {
		if b.from <= i && i <= b.to {
			return true
		}
	}

	for i := b.from; i <= b.to; i++ {
		if a.from <= i && i <= a.to {
			return true
		}
	}
	return false
}

func contain(a, b assignment) bool {
	return (a.from <= b.from && a.to >= b.to) || (b.from <= a.from && b.to >= a.to)
}

func solveOne(pairs []pair) int {
	total := 0
	for _, p := range pairs {
		if contain(p.left, p.right) {
			total += 1
		}
	}

	return total
}

func solveTwo(pairs []pair) int {
	total := 0

	for _, p := range pairs {
		if overlap(p.left, p.right) {
			total += 1
		}
	}

	return total
}

func main() {
	vals := readFile("data.txt")

	fmt.Printf("Solution 1: %d\n", solveOne(vals))
	fmt.Printf("Solution 2: %d\n", solveTwo(vals))
}
