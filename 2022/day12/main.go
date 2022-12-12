package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

type coordinate struct {
	x, y int
}

type queue struct {
	items []node
}

func (q *queue) insert(insert node) {
	if len(q.items) == 0 {
		q.items = append(q.items, insert)
		return
	}

	for i, val := range q.items {
		if val.distance > insert.distance {
			if i == 0 {
				q.items = append([]node{insert}, q.items...)
			} else {
				q.items = append(q.items[:i], q.items[i:]...)
				q.items[i] = insert
			}
			return
		}
	}

	q.items = append(q.items, insert)
}

func (q *queue) remove() node {
	v := q.items[0]
	q.items = q.items[1:]

	return v
}

func (q *queue) empty() bool {
	return len(q.items) == 0
}

type node struct {
	node     coordinate
	distance int
}

func height(r rune) int {
	switch r {
	case 'S':
		return int('a')
	case 'E':
		return int('z')
	default:
		return int(r)
	}
}

func possibleNext(grid [][]rune, current coordinate) []coordinate {
	adjacend := []coordinate{
		{x: current.x + 1, y: current.y},
		{x: current.x - 1, y: current.y},
		{x: current.x, y: current.y + 1},
		{x: current.x, y: current.y - 1},
	}

	var output []coordinate

	for _, a := range adjacend {

		if a.x >= 0 && a.x <= len(grid[0])-1 && a.y >= 0 && a.y <= len(grid)-1 {
			if height(grid[current.y][current.x])+1 >= height(grid[a.y][a.x]) {
				output = append(output, a)
			}
		}
	}

	return output
}

func findStartStop(grid [][]rune) (coordinate, coordinate) {
	start := coordinate{}
	stop := coordinate{}

	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[y]); x++ {
			switch grid[y][x] {
			case 'S':
				start = coordinate{x: x, y: y}
			case 'E':
				stop = coordinate{x: x, y: y}
			default:
				continue
			}
		}
	}

	return start, stop
}

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

func shortestPath(grid [][]rune, start, stop coordinate) int {
	visited := make(map[coordinate]bool)
	dist := make(map[coordinate]int)
	prev := make(map[coordinate]coordinate)

	q := queue{items: []node{}}

	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[y]); x++ {
			dist[coordinate{x: x, y: y}] = math.MaxInt
		}
	}
	dist[start] = 0
	q.insert(node{node: start, distance: 0})

	for !q.empty() {
		current := q.remove()
		if v, ok := visited[current.node]; ok && v {
			continue
		}

		visited[current.node] = true
		next := possibleNext(grid, current.node)

		for _, n := range next {
			if !visited[n] {
				if dist[current.node]+1 < dist[n] {
					dist[n] = dist[current.node] + 1
					prev[n] = current.node
					q.insert(node{node: n, distance: dist[current.node] + 1})
				}
			}
		}
	}

	return dist[stop]
}

func solveOne(grid [][]rune) int {
	start, stop := findStartStop(grid)

	return shortestPath(grid, start, stop)
}

func solveTwo(grid [][]rune) int {
	_, stop := findStartStop(grid)

	shortest := math.MaxInt
	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[y]); x++ {
			if grid[y][x] == 'a' || grid[y][x] == 'S' {
				pathLen := shortestPath(grid, coordinate{x: x, y: y}, stop)
				if pathLen < shortest {
					shortest = pathLen
				}
			}
		}
	}

	return shortest
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFile("data.txt")))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFile("data.txt")))
}
