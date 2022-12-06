package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strings"
)

func assertNoErr(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func readFile(fpath string) string {

	fileContent, err := ioutil.ReadFile(fpath)
	assertNoErr(err)

	return string(fileContent)
}

func findFirstUniqueRuneSeq(str string, length int) int {
	for i := 0; i < len(str)-length; i++ {
		substr := str[i : i+length]
		unique := true

		for x := 0; x < length; x++ {
			unique = unique && strings.Count(substr, string(substr[x])) == 1
		}

		if unique {
			return i + length
		}
	}

	return -1
}

func solveOne(str string) int {
	return findFirstUniqueRuneSeq(str, 4)
}

func solveTwo(str string) int {
	return findFirstUniqueRuneSeq(str, 14)
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFile("data.txt")))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFile("data.txt")))
}
