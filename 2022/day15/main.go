package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"unicode"
)

type coordinate struct {
	x, y int
}

type pair struct {
	beacon coordinate
	sensor coordinate
}

func minMax(pairs []pair) (int, int) {
	maxSensor := math.MinInt
	minSensor := math.MaxInt

	for _, p := range pairs {
		d := distance(p.sensor, p.beacon)
		if p.sensor.x+d > maxSensor {
			maxSensor = p.sensor.x + d
		}
		if p.sensor.x-d < minSensor {
			minSensor = p.sensor.x - d
		}
	}

	return minSensor, maxSensor
}

func abs(x int) int {
	if x >= 0 {
		return x
	}
	return -x
}

func distance(a, b coordinate) int {
	return abs(a.x-b.x) + abs(a.y-b.y)
}

func coveredBySensor(pairs []pair, point coordinate) bool {
	for _, p := range pairs {
		if p.beacon.x == point.x && p.beacon.y == point.y {
			return false
		}

		beaconDistance := distance(p.sensor, p.beacon)
		pointDistance := distance(p.sensor, point)

		if pointDistance <= beaconDistance {
			return true
		}
	}

	return false
}

func coveredBySensorOrSensor(pairs []pair, point coordinate) bool {
	for _, p := range pairs {
		if p.beacon.x == point.x && p.beacon.y == point.y {
			return true
		}

		beaconDistance := distance(p.sensor, p.beacon)
		pointDistance := distance(p.sensor, point)

		if pointDistance <= beaconDistance {
			return true
		}
	}

	return false
}

func perimeter(p pair) []coordinate {
	var output []coordinate
	d := distance(p.sensor, p.beacon)
	for y := p.sensor.y - d - 1; y <= p.sensor.y+d+1; y++ {
		rest := d - abs(p.sensor.y-y) + 1
		output = append(output, coordinate{x: p.sensor.x - rest, y: y})
		if rest > 0 {
			output = append(output, coordinate{x: p.sensor.x + rest, y: y})
		}
	}
	return output
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

func readFile(fpath string) []pair {

	file, err := os.Open(fpath)
	if err != nil {
		fmt.Printf("Error opening file [%s]: %s", fpath, err)
		os.Exit(1)
	}
	defer file.Close()

	var output []pair
	sc := bufio.NewScanner(file)
	sc.Split(bufio.ScanLines)
	for sc.Scan() {
		line := sc.Text()
		sensorX, end, err := extractInt(line, 0)
		assertNoErr(err)
		sensorY, end, err := extractInt(line, end)
		assertNoErr(err)
		beaconX, end, err := extractInt(line, end)
		assertNoErr(err)
		beaconY, end, err := extractInt(line, end)
		assertNoErr(err)

		output = append(output, pair{sensor: coordinate{sensorX, sensorY}, beacon: coordinate{x: beaconX, y: beaconY}})
	}

	return output
}

func solveOne(pairs []pair, y int) int {
	score := 0
	min, max := minMax(pairs)

	for x := min; x <= max; x++ {
		if coveredBySensor(pairs, coordinate{x: x, y: y}) {
			score++
		}
	}

	return score
}

func solveTwo(pairs []pair, min, max int) int {
	var perimeters []coordinate

	for _, p := range pairs {
		perimeters = append(perimeters, perimeter(p)...)
	}

	for _, per := range perimeters {
		if per.x < min || per.x > max || per.y < min || per.y > max {
			continue
		}

		if !coveredBySensorOrSensor(pairs, per) {
			return per.x*4000000 + per.y
		}
	}

	return -1
}

func main() {
	fmt.Printf("Solution 1: %d\n", solveOne(readFile("data.txt"), 2000000))
	fmt.Printf("Solution 2: %d\n", solveTwo(readFile("data.txt"), 0, 4000000))
}
