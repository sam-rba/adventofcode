package main

import (
	"fmt"
	"github.com/sam-rba/adventofcode/lib"
	"golang.org/x/sync/errgroup"
	"os"
	"runtime"
	"slices"
)

const seqLen = 4 // price change sequence length.

type PricePoint struct {
	changes [seqLen]int // the sequence of price changes up to this point.
	price   int
}

func main() {
	initialSecrets := make(chan int)
	go parse(os.Stdin, initialSecrets)

	prices, maxPriceChan := make(chan PricePoint), make(chan int)
	go maxPrice(prices, maxPriceChan)

	var group, commGroup errgroup.Group
	group.SetLimit(3 * runtime.NumCPU())
	for secret := range initialSecrets {
		c, unique := make(chan PricePoint), make(chan PricePoint)
		group.Go(func() error {
			allPrices(secret, c)
			return nil
		})
		go lib.UniqueFunc(c, unique, cmpPricePoint)
		commGroup.Go(func() error {
			forward(unique, prices)
			return nil
		})
	}
	group.Wait()
	commGroup.Wait()
	close(prices)

	fmt.Println("gold:", <-maxPriceChan)
}

func price(secret int) int {
	return secret % 10
}

func allPrices(secret int, prices chan<- PricePoint) {
	defer close(prices)

	var changes [seqLen]int
	lastPrice := price(secret)
	for i := 1; i <= numSecrets; i++ {
		secret = next(secret)
		diff := price(secret) - lastPrice
		lshift(changes[:])
		changes[seqLen-1] = diff
		if i >= seqLen {
			prices <- PricePoint{changes, price(secret)}
		}
		lastPrice = price(secret)
	}
}

func lshift[T any](s []T) {
	for i := 1; i < len(s); i++ {
		s[i-1] = s[i]
	}
}

func maxPrice(prices <-chan PricePoint, max chan<- int) {
	defer close(max)

	totalPrice := make(map[[seqLen]int]int)
	for price := range prices {
		totalPrice[price.changes] = totalPrice[price.changes] + price.price
	}

	maxPrice := 0
	for _, price := range totalPrice {
		if price > maxPrice {
			maxPrice = price
		}
	}
	max <- maxPrice
}

func forward[T any](in <-chan T, out chan<- T) {
	for v := range in {
		out <- v
	}
}

func cmpPricePoint(a, b PricePoint) int {
	return slices.Compare(a.changes[:], b.changes[:])
}
