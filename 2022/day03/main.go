package main

import (
	"bufio"
	"fmt"
	"os"
)

func readFile(fpath string) [][]rune {
	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output [][]rune
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()
		output = append(output, []rune(line))
	}

	return output
}

func intersection[T comparable](a []T, b []T) []T {
	common := make([]T, 0)
	hashed := make(map[T]struct{})

	for _, v := range a {
		hashed[v] = struct{}{}
	}

	for _, v := range b {
		if _, ok := hashed[v]; ok {
			common = append(common, v)
		}
	}

	return common
}

func itemScore(item rune) int {
	if item >= 'a' && item <= 'z' {
		return int(item-'a') + 1
	} else if item >= 'A' && item <= 'Z' {
		return int(item) - 38
	}
	return 0
}

func solveOne(sacks [][]rune) int {
	total := 0
	for _, sack := range sacks {
		left := sack[:len(sack)/2]
		right := sack[len(sack)/2:]

		i := intersection(left, right)
		total += itemScore(i[0])
	}

	return total
}

func solveTwo(sacks [][]rune) int {
	total := 0
	var group [][]rune

	for _, sack := range sacks {
		if len(group) < 2 {
			group = append(group, sack)
			continue
		}

		group = append(group, sack)

		first := intersection(group[0], group[1])
		final := intersection(first, group[2])
		total += itemScore(final[0])

		group = nil
	}

	return total
}

func main() {
	vals := readFile("data.txt")

	fmt.Printf("Solution 1: %d\n", solveOne(vals))
	fmt.Printf("Solution 2: %d\n", solveTwo(vals))
}
