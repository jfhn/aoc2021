package main

import (
	"fmt"
	"bufio"
	"os"
	"log"
)

func main() {
	fmt.Println("Part 1: ", solve1());
	fmt.Println("Part 2: ", solve2());
}

func getInput(filepath string) []string {
	file, err := os.Open(filepath);
	if err != nil {
		log.Fatal(err);
	}
	defer file.Close();

	scanner := bufio.NewScanner(file);

	var input []string;

	for scanner.Scan() {
		input = append(input, scanner.Text());
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err);
	}

	return input;
}

func solve1() int {
	input := getInput("input.txt");

	gammaRate   := 0;
	epsilonRate := 0;

	// if bit == 1 then +1 else -1
	commonBits := make([]int, len(input[0]), len(input[0]));

	bitToValue := func (c rune) int {
		if c == '1' {
			return 1;
		} else {
			return -1;
		}
	}

	for _, line := range input {
		for i, c := range line {
			commonBits[i] += bitToValue(c);
		}
	}

	for i, v := range commonBits {
		if v > 0 {
			gammaRate = gammaRate + (1 << (11 - i));
		} else {
			epsilonRate = epsilonRate + (1 << (11 - i));
		}
	}

	return gammaRate * epsilonRate;
}

func abs(n int) int {
	if n < 0 {
		return -n;
	}
	return n;
}

func binstrToInt(str string) int {
	strlen := len(str);
	result := 0;
	for i, v := range str {
		if v == '1' {
			result = result + (1 << (strlen - i - 1));
		}
	}
	return result;
}

func solve2() int {
	input := getInput("input.txt");

	inputLength := len(input);
	lineLength  := len(input[0]);

	o2Numbers  := make([]int, inputLength, inputLength);
	co2Numbers := make([]int, inputLength, inputLength);

	bit := 0;

	// Keep all numbers at the beginning.
	for i := 0; i < len(input); i++ {
		o2Numbers[i]  = i;
		co2Numbers[i] = i;
	}

	o2Len  := len(o2Numbers);
	co2Len := len(co2Numbers);

	intToChar := func (num int) byte {
		return byte(num + '0');
	}

	getMostCommonValue := func (bit int, len int, numbers []int) int {
		// positive or zero = 1, negative = 0
		acc := 0;
		for i := 0; i < len; i++ {
			num := numbers[i];
			if input[num][bit] == '1' {
				acc += 1;
			} else {
				acc -= 1;
			}
		}

		if acc >= 0 {
			return 1;
		} else {
			return 0;
		}
	}

	updateNumbers := func (bit int, num int, numbers []int, len int) int {
		readIdx  := 0;
		writeIdx := 0;
		newLen   := 0;

		for readIdx < len {
			if input[numbers[readIdx]][bit] == intToChar(num) {
				numbers[writeIdx] = numbers[readIdx];
				writeIdx++;
				newLen++;
			}

			readIdx++;
		}

		return newLen;
	}

	for bit < lineLength && o2Len > 1 {
		if bit >= lineLength && o2Len > 1 {
			log.Fatal("This should not happen.");
		}

		num := getMostCommonValue(bit, o2Len, o2Numbers);
		o2Len = updateNumbers(bit, num, o2Numbers, o2Len);

		bit++;
	}

	bit = 0;

	for bit < lineLength && co2Len > 1 {
		if bit >= lineLength && co2Len > 1 {
			log.Fatal("This should not happen.");
		}

		num := abs(getMostCommonValue(bit, co2Len, co2Numbers) - 1);
		co2Len = updateNumbers(bit, num, co2Numbers, co2Len);

		bit++;
	}

	o2Rating  := binstrToInt(input[o2Numbers[0]]);
	co2Rating := binstrToInt(input[co2Numbers[0]]);

	return o2Rating * co2Rating;
}
