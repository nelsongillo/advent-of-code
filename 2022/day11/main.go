package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

type operation struct {
	operator string
	useOld   bool
	value    int
}

func (o *operation) exec(input int) (int, error) {
	switch o.operator {
	case "+":
		if o.useOld {
			return input + input, nil
		}
		return input + o.value, nil
	case "*":
		if o.useOld {
			return input * input, nil
		}
		return input * o.value, nil
	default:
		return -1, errors.New("unknown operation")
	}
}

type test struct {
	divisibleBy int
	monkeyTrue  int
	monkeyFalse int
}

func (t *test) exec(input int) int {
	if input%t.divisibleBy == 0 {
		return t.monkeyTrue
	}
	return t.monkeyFalse
}

type monkey struct {
	items          []int
	op             operation
	t              test
	inspectedItems int
}

func monkeysTurn(monkeys []monkey, idx int, divideByThree bool, commonModulo int) int {
	inspections := 0

	m := &monkeys[idx]

	itemLen := len(m.items)
	for i := 0; i < itemLen; i++ {
		inspections++

		init := m.items[0]
		m.items = m.items[1:]

		item, err := m.op.exec(init)
		assertNoErr(err)

		if divideByThree {
			item /= 3
		} else {
			item = item % commonModulo
		}

		next := m.t.exec(item)
		monkeys[next].items = append(monkeys[next].items, item)
	}

	return inspections
}

func assertNoErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func intsFromStrings(strs []string) []int {
	var output []int

	for _, s := range strs {
		i, _ := strconv.Atoi(s)
		output = append(output, i)
	}

	return output
}

func readFile(fpath string) []monkey {

	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output []monkey
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	curr := monkey{}
	for sc.Scan() {
		line := sc.Text()

		line = strings.Trim(line, " ")

		if line == "" {
			continue
		}

		if strings.HasPrefix(line, "Monkey") {
			curr = monkey{}
			continue
		}

		if strings.HasPrefix(line, "Starting") {
			line = strings.TrimPrefix(line, "Starting items: ")
			itemsStr := strings.Split(line, ", ")
			items := intsFromStrings(itemsStr)
			curr.items = items
			continue
		}

		if strings.HasPrefix(line, "Operation") {
			line = strings.TrimPrefix(line, "Operation: new = ")
			split := strings.Split(line, " ")
			useOld := strings.EqualFold(split[0], split[2])
			value := 0
			if !useOld {
				v, err := strconv.Atoi(split[2])
				assertNoErr(err)
				value = v
			}
			curr.op = operation{
				operator: split[1],
				value:    value,
				useOld:   useOld,
			}

			continue
		}

		if strings.HasPrefix(line, "Test") {
			split := strings.Split(line, " ")
			value, err := strconv.Atoi(split[3])
			assertNoErr(err)

			curr.t = test{
				divisibleBy: value,
				monkeyTrue:  0,
				monkeyFalse: 0,
			}
			continue
		}

		if strings.HasPrefix(line, "If true") {
			split := strings.Split(line, " ")
			value, err := strconv.Atoi(split[5])
			assertNoErr(err)

			curr.t.monkeyTrue = value
			continue
		}

		if strings.HasPrefix(line, "If false") {
			split := strings.Split(line, " ")
			value, err := strconv.Atoi(split[5])
			assertNoErr(err)

			curr.t.monkeyFalse = value

			output = append(output, curr)
			continue
		}
	}

	return output
}

func solve(monkeys []monkey, rounds int, divideByThree bool) int {
	inspections := make([]int, len(monkeys))

	commonModulo := 1
	for _, m := range monkeys {
		commonModulo *= m.t.divisibleBy
	}

	for round := 0; round < rounds; round++ {
		for idx := range monkeys {
			inspections[idx] += monkeysTurn(monkeys, idx, divideByThree, commonModulo)
		}
	}
	sort.Ints(inspections)
	n := len(inspections)
	return inspections[n-1] * inspections[n-2]
}

func main() {
	fmt.Printf("Solution 1: %d\n", solve(readFile("data.txt"), 20, true))
	fmt.Printf("Solution 2: %d\n", solve(readFile("data.txt"), 10000, false))
}
