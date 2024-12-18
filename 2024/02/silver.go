package main

func (r Report) isSafe() bool {
	if len(r.levels) < 2 {
		return true
	}

	var steps <-chan Step
	if r.levels[1] > r.levels[0] {
		steps = forward(r.levels)
	} else {
		steps = reverse(r.levels)
	}

	for step := range steps {
		size := step.size()
		if size < minStep || size > maxStep {
			return false
		}
	}
	return true
}
