package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Stack struct {
	values []rune
}

func (s *Stack) push(value rune) {
	s.values = append(s.values, value)
}

func (s *Stack) pushMultiple(values []rune) {
	s.values = append(s.values, values...)
}

func (s *Stack) pop() (rune, error) {
	if len(s.values) == 0 {
		return rune(0), errors.New("stack empty")
	}

	out := s.values[len(s.values)-1]
	s.values = s.values[:len(s.values)-1]

	return out, nil
}

func (s *Stack) popMultiple(count int) ([]rune, error) {
	if len(s.values) < count {
		return []rune{}, errors.New("stack too small")
	}

	out := s.values[len(s.values)-count : len(s.values)]
	s.values = s.values[:len(s.values)-count]
	return out, nil
}

func (s *Stack) reverse() {
	var output []rune

	for i := len(s.values) - 1; i >= 0; i-- {
		output = append(output, s.values[i])
	}

	s.values = output
}

type instruction struct {
	amount int
	from   int
	to     int
}

type data struct {
	stacks      []Stack
	instruction []instruction
}

func (d *data) execSingle() []rune {

	for _, i := range d.instruction {
		for x := 0; x < i.amount; x++ {
			value, err := d.stacks[i.from-1].pop()
			assertNoErr(err)

			d.stacks[i.to-1].push(value)
		}
	}

	return d.takeTop()
}

func (d *data) execMultiple() []rune {

	for _, i := range d.instruction {
		values, err := d.stacks[i.from-1].popMultiple(i.amount)
		assertNoErr(err)

		d.stacks[i.to-1].pushMultiple(values)
	}

	return d.takeTop()
}

func (d *data) takeTop() []rune {
	var out []rune
	for _, s := range d.stacks {
		v, err := s.pop()
		assertNoErr(err)
		out = append(out, v)
	}

	return out
}

func assertNoErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func readFile(fpath string) data {
	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	output := data{stacks: []Stack{}, instruction: []instruction{}}
	parseInstructions := false
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()

		if line == "" {
			parseInstructions = true
			continue
		}

		if parseInstructions {
			cmd := strings.Split(line, " ")

			a, err := strconv.Atoi(cmd[1])
			assertNoErr(err)
			f, err := strconv.Atoi(cmd[3])
			assertNoErr(err)
			t, err := strconv.Atoi(cmd[5])
			assertNoErr(err)

			i := instruction{amount: a, from: f, to: t}

			output.instruction = append(output.instruction, i)
		} else {
			s := []rune(line)

			if s[1] == '1' {
				continue
			}

			for i := 1; i < len(s); i += 4 {
				si := (i - 1) / 4

				for x := len(output.stacks); x <= si; x++ {
					output.stacks = append(output.stacks, Stack{values: []rune{}})
				}

				if s[i] == ' ' {
					continue
				} else {
					output.stacks[si].push(s[i])
				}
			}
		}
	}

	for i := range output.stacks {
		output.stacks[i].reverse()
	}

	return output
}

func solveOne(d data) []rune {
	return d.execSingle()
}

func solveTwo(d data) []rune {
	return d.execMultiple()
}

func main() {
	fmt.Printf("Solution 1: %s\n", string(solveOne(readFile("data.txt"))))
	fmt.Printf("Solution 2: %s\n", string(solveTwo(readFile("data.txt"))))
}
