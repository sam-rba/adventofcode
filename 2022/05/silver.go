package main

func move(stacks []Stack, move Move) {
	stacks[move.to], stacks[move.from] = transfer(stacks[move.to], stacks[move.from], move.n)
}
