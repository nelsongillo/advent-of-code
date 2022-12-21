package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
	"unicode"
)

type operation struct {
	a       string
	b       string
	operand string
}

type monkey struct {
	name    string
	isValue bool
	value   int
	op      *operation
}

func (o *operation) calc(monkeys map[string]monkey) int {
	a := monkeys[o.a]
	b := monkeys[o.b]

	switch o.operand {
	case "+":
		return a.calc(monkeys) + b.calc(monkeys)
	case "-":
		return a.calc(monkeys) - b.calc(monkeys)
	case "*":
		return a.calc(monkeys) * b.calc(monkeys)
	case "/":
		return a.calc(monkeys) / b.calc(monkeys)
	default:
		assertNoErr(errors.New("unknown operand"))
	}
	return -1
}

func (o *operation) calcReverse(monkeys map[string]monkey, target int, isA bool) int {
	a := monkeys[o.a]
	b := monkeys[o.b]

	if isA {
		switch o.operand {
		case "+":
			return target - a.calc(monkeys)
		case "-":
			return a.calc(monkeys) - target
		case "*":
			return target / a.calc(monkeys)
		case "/":
			return a.calc(monkeys) / target
		default:
			assertNoErr(errors.New("unknown operand"))
		}
	} else {
		switch o.operand {
		case "+":
			return target - b.calc(monkeys)
		case "-":
			return target + b.calc(monkeys)
		case "*":
			return target / b.calc(monkeys)
		case "/":
			return target * b.calc(monkeys)
		default:
			assertNoErr(errors.New("unknown operand"))
		}
	}

	return -1
}

func (m *monkey) calc(monkeys map[string]monkey) int {
	if m.isValue {
		return m.value
	}

	m.value = m.op.calc(monkeys)
	m.isValue = true
	return m.value
}

func (m *monkey) walkTo(dst string, monkeys map[string]monkey, target int) (int, error) {
	if m.name == dst {
		return target, nil
	}

	if m.op != nil {
		a := monkeys[m.op.a]
		b := monkeys[m.op.b]

		if value, err := b.walkTo(dst, monkeys, m.op.calcReverse(monkeys, target, true)); err == nil {
			return value, nil
		}

		if value, err := a.walkTo(dst, monkeys, m.op.calcReverse(monkeys, target, false)); err == nil {
			return value, nil
		}
	}

	return -1, errors.New("target monkey not found")
}

func assertNoErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func extractInt(str string, begin int) (int, int, error) {
	var intStr []rune
	asRunes := []rune(str)
	intStarted := false

	for i := begin; i < len(asRunes); i++ {
		if unicode.IsDigit(asRunes[i]) || (!intStarted && asRunes[i] == '-') {
			intStr = append(intStr, asRunes[i])
			intStarted = true
		} else if intStarted {
			value, err := strconv.Atoi(string(intStr))
			return value, i, err
		}
	}

	if intStarted {
		value, err := strconv.Atoi(string(intStr))
		return value, len(str), err
	}

	return -1, len(str), errors.New("no int value found")
}

func readFile(fpath string) map[string]monkey {

	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	output := make(map[string]monkey)
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()

		split := strings.Split(line, ":")
		name := split[0]

		if v, _, err := extractInt(strings.Trim(split[1], " "), 0); err == nil {
			output[name] = monkey{name: name, isValue: true, value: v, op: nil}
			continue
		}

		ops := strings.Split(strings.Trim(split[1], " "), " ")
		output[name] = monkey{name: name, isValue: false, value: -1, op: &operation{a: ops[0], b: ops[2], operand: ops[1]}}
	}

	return output
}

func solveOne(monkeys map[string]monkey) int {
	root := monkeys["root"]
	return root.calc(monkeys)
}

func solveTwo(monkeys map[string]monkey) int {
	root := monkeys["root"]
	root.calc(monkeys)
	a := monkeys[root.op.a]
	b := monkeys[root.op.b]

	if value, err := a.walkTo("humn", monkeys, b.calc(monkeys)); err == nil {
		return value
	}

	if value, err := b.walkTo("humn", monkeys, a.calc(monkeys)); err == nil {
		return value
	}

	return -1
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFile("data.txt")))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFile("data.txt")))
}
