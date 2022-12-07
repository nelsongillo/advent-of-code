package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

type directory struct {
	name   string
	subdir map[string]directory
	files  map[string]int
}

func (d *directory) size() int {
	total := 0

	for _, s := range d.files {
		total += s
	}

	for _, dir := range d.subdir {
		total += dir.size()
	}

	log.Printf("Directory: %s - Size: %d\n", d.name, total)

	return total
}

func (d *directory) findWithMax(max int) int {
	total := 0

	if d.size() <= max {
		total += d.size()
	}

	for _, dir := range d.subdir {
		total += dir.findWithMax(max)
	}

	return total
}

func (d *directory) toSizeList() []int {
	li := []int{
		d.size(),
	}

	for _, v := range d.subdir {
		li = append(li, v.size())

		li = append(li, v.toSizeList()...)
	}

	return li
}

func findToDelete(root directory, toFree int) int {
	var poss []int
	for _, v := range root.toSizeList() {
		if v >= toFree {
			poss = append(poss, v)
		}
	}

	sort.Ints(poss)

	return poss[0]
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

func buildDir(curr directory, data []string) (directory, int) {
	for i := 0; i < len(data); i++ {
		log.Printf("Index: %d - Line: %s\n", i, data[i])

		split := strings.Split(data[i], " ")
		switch split[0] {
		case "$":
			switch split[1] {
			case "cd":
				if split[2] == ".." {
					return curr, i + 1
				} else {
					_, ok := curr.subdir[split[2]]
					if !ok {
						curr.subdir[split[2]] = directory{name: split[2], subdir: map[string]directory{}, files: map[string]int{}}
					}

					_, add := buildDir(curr.subdir[split[2]], data[i+1:])
					i += add
					continue
				}
			case "ls":
			default:
			}
		case "dir":
			if _, ok := curr.subdir[split[1]]; !ok {
				curr.subdir[split[1]] = directory{name: split[1], subdir: map[string]directory{}, files: map[string]int{}}
			}
		default:
			if _, ok := curr.files[split[1]]; !ok {
				size, err := strconv.Atoi(split[0])
				assertNoErr(err)

				curr.files[split[1]] = size
			}
		}
	}

	return curr, len(data)
}

func solveOne(data []string) int {
	root := directory{name: "/", subdir: map[string]directory{}, files: map[string]int{}}
	root, _ = buildDir(root, data[1:])

	return root.findWithMax(100000)
}

func solveTwo(data []string, available, toBeFree int) int {
	root := directory{name: "/", subdir: map[string]directory{}, files: map[string]int{}}
	root, _ = buildDir(root, data[1:])

	currFree := available - root.size()
	toFree := toBeFree - currFree

	return findToDelete(root, toFree)
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFile("data.txt")))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFile("data.txt"), 70000000, 30000000))
}
