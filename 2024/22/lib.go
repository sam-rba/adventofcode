package main

const (
	numSecrets = 2000
	pruneDiv   = 16777216
)

func next(secret int) int {
	secret = prune(mix(secret, secret*64))
	secret = prune(mix(secret, secret/32))
	secret = prune(mix(secret, secret*2048))
	return secret
}

func mix(a, b int) int {
	return a ^ b
}

func prune(a int) int {
	return a % pruneDiv
}
