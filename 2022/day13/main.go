package main

import (
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

type packet struct {
	isValue bool
	value   int
	list    []packet
}

type pair struct {
	left, right packet
}

func (a *packet) compare(b *packet) int {
	// Both are values
	if a.isValue && b.isValue {
		if a.value < b.value {
			return 1
		}
		if a.value > b.value {
			return -1
		}
		return 0
	}

	// both are lists
	if !a.isValue && !b.isValue {
		for i := 0; i < len(a.list) && i < len(b.list); i++ {
			v := a.list[i].compare(&b.list[i])
			if v != 0 {
				return v
			}
		}

		if len(a.list) < len(b.list) {
			return 1
		}
		if len(a.list) > len(b.list) {
			return -1
		}
		return 0
	}

	// Only A is value
	if a.isValue {
		p := packet{isValue: false, value: 0, list: []packet{*a}}
		return p.compare(b)
	}

	// Only b is value
	if b.isValue {
		p := packet{isValue: false, value: 0, list: []packet{*b}}
		return a.compare(&p)
	}

	return 0
}

func (p *pair) isSorted() bool {
	return p.left.compare(&p.right) == 1
}

func parsePacket(line string) packet {
	p, _ := parseSubPacket(line, 0)
	return p
}

func parseSubPacket(line string, begin int) (packet, int) {

	// Not a list
	if line[begin] != '[' {
		end := begin
		for line[end] != ']' && line[end] != ',' {
			end++
		}

		v, err := strconv.Atoi(line[begin:end])
		assertNoErr(err)
		return packet{isValue: true, value: v, list: nil}, end
	}

	begin++
	var list []packet
	for line[begin] != ']' {
		if line[begin] == ',' || line[begin] == ' ' {
			begin++
			continue
		}

		var child packet
		child, begin = parseSubPacket(line, begin)
		list = append(list, child)
	}

	return packet{isValue: false, value: 0, list: list}, begin + 1
}

func assertNoErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func readFileOne(fpath string) []pair {

	file, err := os.ReadFile("data.txt")
	if err != nil {
		fmt.Printf("Error reading file [%s]: %s", fpath, err)
		os.Exit(1)
	}

	var output []pair
	lines := strings.Split(string(file), "\n")
	for i := 0; i < len(lines); i += 2 {
		if lines[i] == "" {
			i--
			continue
		}

		left := parsePacket(lines[i])
		right := parsePacket(lines[i+1])
		output = append(output, pair{left: left, right: right})
	}
	return output
}

func readFileTwo(fpath string) []packet {

	file, err := os.ReadFile("data.txt")
	if err != nil {
		fmt.Printf("Error reading file [%s]: %s", fpath, err)
		os.Exit(1)
	}

	var output []packet
	lines := strings.Split(string(file), "\n")
	for i := 0; i < len(lines); i++ {
		if lines[i] == "" {
			continue
		}

		p := parsePacket(lines[i])
		output = append(output, p)
	}
	return output
}

func solveOne(pairs []pair) int {
	score := 0

	for i, p := range pairs {
		if p.isSorted() {
			score += i + 1
		}
	}

	return score
}

func solveTwo(packets []packet) int {

	div1 := parsePacket("[[2]]")
	div2 := parsePacket("[[6]]")

	packets = append(packets, div1, div2)

	sort.Slice(packets, func(i, j int) bool {
		return packets[i].compare(&packets[j]) == 1
	})

	output := 1
	for i, p := range packets {
		if p.compare(&div1) == 0 || p.compare(&div2) == 0 {
			output *= i + 1
		}
	}

	return output
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFileOne("data.txt")))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFileTwo("data.txt")))
}
