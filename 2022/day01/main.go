package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func readFile(fpath string) [][]int {
	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output [][]int
	var curr []int

	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()
		if strings.EqualFold(line, "") {
			if len(curr) > 0 {
				output = append(output, curr)
				curr = nil
			}
			continue
		}

		val, err := strconv.Atoi(line)
		if err != nil {
			fmt.Printf("Unable to convert [%s] to int", line)
			os.Exit(1)
		}
		curr = append(curr, val)
	}

	return output
}

func sum(vals []int) int {
	total := 0
	for _, val := range vals {
		total += val
	}

	return total
}

func solveOne(vals [][]int) int {
	var summed []int
	for _, elf := range vals {
		summed = append(summed, sum(elf))
	}

	sort.Ints(summed)

	return summed[len(summed)-1]
}

func solveTwo(vals [][]int) int {
	var summed []int
	for _, elf := range vals {
		summed = append(summed, sum(elf))
	}

	sort.Ints(summed)

	return summed[len(summed)-1] + summed[len(summed)-2] + summed[len(summed)-3]
}

func main() {
	vals := readFile("data.txt")

	fmt.Printf("Solution 1: %d\n", solveOne(vals))
	fmt.Printf("Solution 2: %d\n", solveTwo(vals))
}
