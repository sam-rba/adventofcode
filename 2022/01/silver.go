package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	most := 0
	calories := 0
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()

		if line == "" {
			if calories > most {
				most = calories
			}
			calories = 0
			continue
		}

		snack, err := strconv.Atoi(line)
		if err != nil {
			log.Fatal(err)
		}
		calories += snack
	}

	fmt.Println("silver:", most)
}
