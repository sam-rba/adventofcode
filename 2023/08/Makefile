all: p1 p2

p1: p1.go share.go
	go build $^
	gofmt -l -s -w $^

p2: p2.go share.go
	go build $^
	gofmt -l -s -w $^

.PHONY: all
