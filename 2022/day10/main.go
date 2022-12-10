package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const (
	width int = 40
)

var importantCycles = map[int]struct{}{
	20:  {},
	60:  {},
	100: {},
	140: {},
	180: {},
	220: {},
}

type cpu struct {
	cycles int
	x      int
}

func (c *cpu) clock() int {
	output := 0
	if _, ok := importantCycles[c.cycles]; ok {
		output = c.x * c.cycles
	}
	c.drawPixel()
	c.cycles++

	return output
}

func (c *cpu) add(value int) int {
	first := c.clock()
	second := c.clock()

	c.x += value

	return first + second
}

func (c *cpu) drawPixel() {
	index := c.cycles - 1

	if index%width == 0 {
		fmt.Print("\n")
	}

	if index%width >= c.x-1 && index%width <= c.x+1 {
		fmt.Print("#")
	} else {
		fmt.Print(".")
	}
}

func assertNoErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func readFile(fpath string) []string {

	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output []string
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()
		output = append(output, line)
	}

	return output
}

func solve(cmds []string) int {
	total := 0

	c := cpu{cycles: 1, x: 1}

	for _, cmd := range cmds {
		if strings.HasPrefix(cmd, "addx") {
			value, err := strconv.Atoi(strings.Split(cmd, " ")[1])
			assertNoErr(err)
			total += c.add(value)
		} else {
			total += c.clock()
		}
	}

	return total
}

func main() {
	fmt.Printf("\nSolution - Score: %d\n", solve(readFile("data.txt")))
}
