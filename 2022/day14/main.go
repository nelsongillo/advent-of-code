package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

type cave [][]bool

type coordinate struct {
	x, y int
}

func initCave(size int) cave {
	c := make(cave, size)
	for i := range c {
		c[i] = make([]bool, size)
	}

	return c
}

func (c cave) print(x0, x1, y0, y1 int, sand coordinate) {
	for l := y0; l <= y1; l++ {
		for col := x0; col <= x1; col++ {
			if sand.x == col && sand.y == l {
				fmt.Print("o")
			} else if c[l][col] {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
}

func (c cave) drawLine(start coordinate, stop coordinate) {
	// Vertical
	if start.x == stop.x {
		for i := min(start.y, stop.y); i <= max(start.y, stop.y); i++ {
			c[i][start.x] = true
		}
		return
	}

	// Horizontal
	if start.y == stop.y {
		for i := min(start.x, stop.x); i <= max(start.x, stop.x); i++ {
			c[start.y][i] = true
		}
		return
	}

	assertNoErr(errors.New("diagonal lines not supported"))
}

func (c cave) lowest() int {
	low := len(c)
	for y := low - 1; y > 0; y-- {
		for x := 0; x < len(c[y]); x++ {
			if c[y][x] {
				low = y
				break
			}
		}
		if low == y {
			return low
		}
	}

	return low
}

func (c cave) dropSand(sand coordinate, floor bool) bool {
	low := c.lowest()
	start := sand
	for sand.y < low {
		if !c[sand.y+1][sand.x] {
			sand.y++
		} else if !c[sand.y+1][sand.x-1] {
			sand.x--
			sand.y++
		} else if !c[sand.y+1][sand.x+1] {
			sand.x++
			sand.y++
		} else {
			c[sand.y][sand.x] = true
			if floor && sand.x == start.x && sand.y == start.y {
				return true
			}
			return false
		}
	}

	return true
}

func min(values ...int) int {
	m := math.MaxInt

	for _, v := range values {
		if v < m {
			m = v
		}
	}

	return m
}

func max(values ...int) int {
	m := math.MinInt

	for _, v := range values {
		if v > m {
			m = v
		}
	}

	return m
}

func assertNoErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func parseCoordinate(strs []string) coordinate {
	if len(strs) != 2 {
		assertNoErr(errors.New("string array to big"))
	}

	x, err := strconv.Atoi(strs[0])
	assertNoErr(err)
	y, err := strconv.Atoi(strs[1])
	assertNoErr(err)

	return coordinate{x: x, y: y}
}

func readFile(fpath string, caveSize int) cave {
	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	c := initCave(caveSize)
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()

		split := strings.Split(line, " -> ")
		start := parseCoordinate(strings.Split(split[0], ","))
		for i := range split[1:] {
			next := parseCoordinate(strings.Split(split[i+1], ","))
			c.drawLine(start, next)
			start = next
		}
	}

	return c
}

func solveOne(c cave, dropPoint coordinate) int {
	score := 0

	for !c.dropSand(dropPoint, false) {
		score++
	}

	return score
}

func solveTwo(c cave, dropPoint coordinate) int {
	score := 1

	low := c.lowest()
	c.drawLine(coordinate{x: 0, y: low + 2}, coordinate{x: len(c[low]) - 1, y: low + 2})

	for !c.dropSand(dropPoint, true) {
		score++
	}

	return score
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFile("data.txt", 1000), coordinate{500, 0}))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFile("data.txt", 1000), coordinate{500, 0}))
}
