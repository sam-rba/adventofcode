package main

import "slices"

const part = "gold"

func (disc Disc) compact() Disc {
	for c := len(disc) - 1; c >= 0; c-- {
		cluster := disc[c]
		if cluster.free {
			continue
		}

		free, ok := disc.firstFree(cluster.size)
		if !ok || free > c {
			continue
		}

		// Remove file.
		disc = slices.Delete(disc, c, c+1)

		// Split free space.
		if disc[free].size > cluster.size {
			disc = slices.Insert(disc, free+1, Cluster{
				-1,
				disc[free].start + cluster.size,
				disc[free].size - cluster.size,
				true,
			})
		}

		// Insert file into free space.
		start := disc[free].start
		cluster.start = start
		disc[free] = cluster
	}

	disc = disc.trim()

	return disc
}

// First free cluster of at least a certain size.
func (disc Disc) firstFree(size int) (int, bool) {
	for i, cluster := range disc {
		if cluster.free && cluster.size >= size {
			return i, true
		}
	}
	return -1, false
}
