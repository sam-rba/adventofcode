all: silver gold

silver: silver.go rps.go
	go build $^

gold: gold.go rps.go
	go build $^

test: rps.go silver.go silver_test.go
	go test $^

fmt:
	gofmt -l -s -w *.go

.PHONY: test fmt
