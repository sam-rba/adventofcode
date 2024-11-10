package main

func move(stacks []Stack, move Move) {
	var tmp Stack
	tmp, stacks[move.from] = transfer(tmp, stacks[move.from], move.n)
	stacks[move.to], _ = transfer(stacks[move.to], tmp, move.n)
}
