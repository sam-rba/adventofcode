package main

import (
	"slices"
	"testing"
)

func TestMix(t *testing.T) {
	got := mix(42, 15)
	want := 37
	if got != want {
		t.Errorf("got %v; want %v", got, want)
	}
}

func TestPrune(t *testing.T) {
	got := prune(100000000)
	want := 16113920
	if got != want {
		t.Errorf("got %v; want %v", got, want)
	}
}

func TestFirstTen(t *testing.T) {
	want := []int{15887950,
		16495136,
		527345,
		704524,
		1553684,
		12683156,
		11100544,
		12249484,
		7753432,
		5908254,
	}

	secret := 123
	got := make([]int, 10)
	for i := 0; i < 10; i++ {
		secret = next(secret)
		got[i] = secret
	}

	if !slices.Equal(got, want) {
		t.Errorf("got %v; want %v", got, want)
	}
}
