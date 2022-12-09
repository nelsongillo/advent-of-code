package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func isVisible(forest [][]int, x, y int) bool {
	// Tree is on the forest edge
	if x == 0 || x == len(forest[0])-1 || y == 0 || y == len(forest)-1 {
		return true
	}

	return isVisibleRow(forest, x, y) || isVisibleColumn(forest, x, y)
}

func isVisibleRow(forest [][]int, x, y int) bool {
	visibleLeft := true
	visibleRight := true

	for rx := 0; rx < x; rx++ {
		if forest[x][y] <= forest[rx][y] {
			visibleLeft = false
		}
	}

	for rx := x + 1; rx < len(forest); rx++ {
		if forest[x][y] <= forest[rx][y] {
			visibleRight = false
		}
	}
	return visibleLeft || visibleRight
}

func isVisibleColumn(forest [][]int, x, y int) bool {
	visibleTop := true
	visibleBottom := true

	for cy := 0; cy < y; cy++ {
		if forest[x][y] <= forest[x][cy] {
			visibleTop = false
		}
	}

	for cy := y + 1; cy < len(forest); cy++ {
		if forest[x][y] <= forest[x][cy] {
			visibleBottom = false
		}
	}
	return visibleTop || visibleBottom
}

func viewingScore(forest [][]int, x, y int) int {
	row := viewingScoreRow(forest, x, y)
	col := viewingScoreColumn(forest, x, y)

	return row * col
}

func viewingScoreRow(forest [][]int, x, y int) int {
	scoreLeft := 0
	scoreRight := 0

	if x != 0 {
		for rx := x - 1; rx >= 0; rx-- {
			scoreLeft += 1
			if forest[y][x] <= forest[y][rx] {
				break
			}
		}
	}

	if x != len(forest[y])-1 {
		for rx := x + 1; rx < len(forest[y]); rx++ {
			scoreRight += 1
			if forest[y][x] <= forest[y][rx] {
				break
			}
		}
	}

	return scoreLeft * scoreRight
}

func viewingScoreColumn(forest [][]int, x, y int) int {
	scoreTop := 0
	scoreBottom := 0

	if y != 0 {
		for cy := y - 1; cy >= 0; cy-- {
			scoreTop += 1
			if forest[y][x] <= forest[cy][x] {
				break
			}
		}
	}

	if y != len(forest)-1 {
		for cy := y + 1; cy < len(forest); cy++ {
			scoreBottom += 1
			if forest[y][x] <= forest[cy][x] {
				break
			}
		}
	}

	return scoreTop * scoreBottom
}

func intsFromStrings(strs []string) []int {
	var output []int

	for _, s := range strs {
		i, _ := strconv.Atoi(s)
		output = append(output, i)
	}

	return output
}

func readFile(fpath string) [][]int {

	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output [][]int
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()
		output = append(output, intsFromStrings(strings.Split(line, "")))
	}

	return output
}

func solveOne(forest [][]int) int {
	total := 0

	for x := 0; x < len(forest); x++ {
		for y := 0; y < len(forest[x]); y++ {
			if isVisible(forest, x, y) {
				total += 1
			}
		}
	}

	return total
}

func solveTwo(forest [][]int) int {
	maxScore := 0

	for y := 0; y < len(forest); y++ {
		for x := 0; x < len(forest[y]); x++ {
			score := viewingScore(forest, x, y)
			if score > maxScore {
				maxScore = score
			}
		}
	}

	return maxScore
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFile("data.txt")))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFile("data.txt")))
}
