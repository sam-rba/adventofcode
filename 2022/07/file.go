package main

type File struct {
	name string
	size int
}

func (f *File) IsDir() bool {
	return false
}

func (f *File) Name() string {
	return f.name
}

func (f *File) Size() int {
	return f.size
}
