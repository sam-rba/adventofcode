package main

import (
	"fmt"
	"github.com/mazznoer/colorgrad"
	"github.com/sam-rba/adventofcode/lib"
	"image"
	"image/png"
	"io"
	"log"
	"os"
	"slices"
)

type Region struct {
	plant byte
	plots []lib.Point
}

func main() {
	grid, err := parse(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}

	if err := writeImg(grid); err != nil {
		log.Fatal(err)
	}

	visited := make([][]bool, len(grid))
	for i := range grid {
		visited[i] = make([]bool, len(grid[0]))
	}
	var regions []Region
	fill(lib.Point{0, 0}, grid, visited, &regions)
	fmt.Println(len(regions))
	total := 0
	for _, region := range regions {
		price := region.price()
		fmt.Printf("%c a%d p%d $%d\n", region.plant, region.area(), region.perimeter(), price)
		total += price
	}
	fmt.Println("silver:", total)

}

func parse(in io.Reader) ([][]byte, error) {
	var grid [][]byte
	buf := make([]byte, 1)
	var err error
Loop:
	for y := 0; ; y++ {
		grid = append(grid, []byte{})
		for x := 0; ; x++ {
			if _, err = in.Read(buf); err != nil {
				break Loop
			}
			if buf[0] == '\n' {
				break
			}
			grid[y] = append(grid[y], buf[0])
		}
	}
	if len(grid) > 0 && len(grid[len(grid)-1]) == 0 {
		grid = grid[:len(grid)-1]
	}
	if err == io.EOF {
		return grid, nil
	}
	return grid, err
}

func fill(start lib.Point, grid [][]byte, visited [][]bool, regions *[]Region) {
	region := Region{
		grid[start.Y][start.X],
		[]lib.Point{},
	}

	var regionQueue []lib.Point

	queue := []lib.Point{start}
	for len(queue) > 0 {
		p := queue[len(queue)-1]
		queue = queue[:len(queue)-1]

		if visited[p.Y][p.X] || region.contains(p) || inAnyRegion(p, *regions) {
			continue
		}

		if grid[p.Y][p.X] != region.plant {
			if i, ok := slices.BinarySearchFunc(regionQueue, p, lib.CmpPoint); !ok {
				regionQueue = slices.Insert(regionQueue, i, p)
			}
			continue
		}

		region.add(p)

		visited[p.Y][p.X] = true

		if p.Y > 0 {
			queue = append(queue, lib.Point{p.X, p.Y - 1})
		}
		if p.X < len(grid[p.Y])-1 {
			queue = append(queue, lib.Point{p.X + 1, p.Y})
		}
		if p.Y < len(grid)-1 {
			queue = append(queue, lib.Point{p.X, p.Y + 1})
		}
		if p.X > 0 {
			queue = append(queue, lib.Point{p.X - 1, p.Y})
		}
	}

	if len(region.plots) > 0 {
		*regions = append(*regions, region)
	}

	for y := range visited {
		for x := range visited[y] {
			visited[y][x] = false
		}
	}
	for _, start := range regionQueue {
		fill(start, grid, visited, regions)
	}
}

func (r Region) contains(p lib.Point) bool {
	_, ok := slices.BinarySearchFunc(r.plots, p, lib.CmpPoint)
	return ok
}

func (r *Region) add(p lib.Point) {
	if i, ok := slices.BinarySearchFunc(r.plots, p, lib.CmpPoint); !ok {
		r.plots = slices.Insert(r.plots, i, p)
	}
}

func inAnyRegion(p lib.Point, regions []Region) bool {
	for _, region := range regions {
		if region.contains(p) {
			return true
		}
	}
	return false
}

func (r Region) price() int {
	return r.area() * r.perimeter()
}

func (r Region) area() int {
	return len(r.plots)
}

func (r Region) perimeter() int {
	return 4*r.area() - 2*r.numEdges()
}

func (r Region) numEdges() int {
	e := 0
	for _, p := range r.plots {
		neighbors := []lib.Point{
			{p.X, p.Y - 1},
			{p.X + 1, p.Y},
			{p.X, p.Y + 1},
			{p.X - 1, p.Y},
		}
		for _, n := range neighbors {
			if r.contains(n) {
				e++
			}
		}
	}
	return e / 2
}

func writeImg(grid [][]byte) error {
	grad, err := colorgrad.NewGradient().
		Domain('A', 'Z').
		Colors(colorgrad.Rainbow().Colors('Z' - 'A' + 1)...).
		Build()
	if err != nil {
		return err
	}
	grad = grad.Sharp('Z'-'A'+1, 0)
	img := image.NewRGBA(image.Rect(0, 0, len(grid[0]), len(grid)))
	for y := range grid {
		for x := range grid[0] {
			col := grad.At(float64(grid[y][x]))
			img.Set(x, y, col)
		}
	}
	f, err := os.Create("img.png")
	if err != nil {
		return err
	}
	defer f.Close()
	return png.Encode(f, img)
}
