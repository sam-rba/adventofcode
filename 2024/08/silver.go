package main

const part = "silver"

func findAntinodes(a, b Antenna, bounds Rectangle) []Point {
	if a.freq != b.freq {
		return []Point{}
	}

	v := vpt(a.pos, b.pos)
	antinodes := make([]Point, 0, 2)
	for _, antinode := range []Point{a.pos.sub(v), b.pos.add(v)} {
		if ptInRect(antinode, bounds) {
			antinodes = append(antinodes, antinode)
		}
	}
	return antinodes
}
