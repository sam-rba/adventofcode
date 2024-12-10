package main

import "container/heap"

type Tile struct {
	pos      Coord
	distance int
	index    int
}

type PriorityQueue []*Tile

func (pq PriorityQueue) Len() int { return len(pq) }

func (pq PriorityQueue) Less(i, j int) bool {
	return pq[i].distance < pq[j].distance
}

func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}

func (pq *PriorityQueue) Push(x any) {
	n := len(*pq)
	tile := x.(*Tile)
	tile.index = n
	*pq = append(*pq, tile)
}

func (pq *PriorityQueue) Pop() any {
	old := *pq
	n := len(old)
	tile := old[n-1]
	old[n-1] = nil
	tile.index = -1
	*pq = old[:n-1]
	return tile
}

func (pq *PriorityQueue) update(tile *Tile, distance int) {
	tile.distance = distance
	heap.Fix(pq, tile.index)
}
