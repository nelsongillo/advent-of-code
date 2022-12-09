package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type direction int

const (
	Left direction = iota
	Right
	Up
	Down
)

type move struct {
	direction direction
	amount    int
}

type coordinate struct {
	x int
	y int
}

func (a *coordinate) walk(d direction) {
	switch d {
	case Up:
		a.y++
	case Down:
		a.y--
	case Left:
		a.x--
	case Right:
		a.x++
	default:
		log.Fatalf("unknown direction %d", d)
	}
}

func (a *coordinate) follow(b *coordinate) {
	diffX := a.x - b.x
	diffY := a.y - b.y
	if abs(diffX) <= 1 && abs(diffY) <= 1 {
		return
	}

	a.x -= sign(diffX)
	a.y -= sign(diffY)
}

func sign(x int) int {
	if x == 0 {
		return 0
	} else if x > 0 {
		return 1
	}
	return -1
}

func abs(x int) int {
	if x >= 0 {
		return x
	}
	return -x
}

func directionFromString(str string) direction {
	switch str {
	case "R":
		return Right
	case "L":
		return Left
	case "U":
		return Up
	case "D":
		return Down
	default:
		log.Fatalf("unknown direction %s", str)
	}
	return -1
}

func assertNoErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func readFile(fpath string) []move {

	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output []move
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()
		split := strings.Split(line, " ")
		count, err := strconv.Atoi(split[1])
		assertNoErr(err)

		output = append(output, move{direction: directionFromString(split[0]), amount: count})
	}

	return output
}

func solveOne(moves []move) int {
	head := coordinate{}
	tail := coordinate{}

	visited := map[coordinate]struct{}{
		tail: {},
	}

	for _, m := range moves {
		for i := 0; i < m.amount; i++ {
			head.walk(m.direction)
			tail.follow(&head)
			visited[tail] = struct{}{}
		}
	}

	return len(visited)
}

func solveTwo(moves []move) int {
	rope := [10]coordinate{}

	visited := map[coordinate]struct{}{
		coordinate{x: 0, y: 0}: {},
	}

	for _, m := range moves {
		for i := 0; i < m.amount; i++ {
			rope[0].walk(m.direction)

			for x := 1; x < 10; x++ {
				rope[x].follow(&rope[x-1])
			}
			visited[rope[9]] = struct{}{}
		}
	}

	return len(visited)
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFile("data.txt")))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFile("data.txt")))
}
