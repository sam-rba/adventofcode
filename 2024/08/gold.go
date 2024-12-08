package main

const part = "gold"

func findAntinodes(a, b Antenna, bounds Rectangle) []Point {
	if a.freq != b.freq {
		return []Point{}
	}

	v := vpt(a.pos, b.pos)
	var antinodes []Point
	for p := a.pos; ptInRect(p, bounds); p = p.sub(v) {
		antinodes = append(antinodes, p)
	}
	for p := b.pos; ptInRect(p, bounds); p = p.add(v) {
		antinodes = append(antinodes, p)
	}
	return antinodes
}
