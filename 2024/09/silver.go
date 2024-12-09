package main

import "slices"

const part = "silver"

func (disc Disc) compact() Disc {
	disc = disc.trim()

	for c := 0; c < len(disc); c++ {
		if !disc[c].free {
			continue
		}

		var cluster Cluster
		cluster, disc = disc.pop(disc[c].size)
		cluster.start = disc[c].start

		disc[c].start += cluster.size
		disc[c].size -= cluster.size

		if disc[c].size <= 0 {
			disc[c] = cluster
		} else {
			disc = slices.Insert(disc, c, cluster)
		}

		disc = disc.trim()
	}

	return disc
}

func (disc Disc) pop(size int) (Cluster, Disc) {
	cluster := disc[len(disc)-1]

	if cluster.free {
		panic("empty cluster at end of disc")
	}

	if cluster.size <= size {
		disc = disc[:len(disc)-1]
	} else {
		cluster.size = size
		disc[len(disc)-1].size -= size
	}

	return cluster, disc
}
