package main

type DirEntry interface {
	IsDir() bool
	Name() string
	Size() int
}
