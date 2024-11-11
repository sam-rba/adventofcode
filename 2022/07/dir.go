package main

import "fmt"

type Dir struct {
	name    string
	entries map[string]DirEntry
}

func newDir(name string) *Dir {
	d := &Dir{
		name,
		make(map[string]DirEntry),
	}
	d.entries["."] = d
	return d
}

func (d *Dir) IsDir() bool {
	return true
}

func (d *Dir) Name() string {
	return d.name
}

func (d *Dir) Size() int {
	var sizes []chan int
	for name, entry := range d.entries {
		if name == "." || name == ".." {
			continue
		}
		c := make(chan int)
		sizes = append(sizes, c)
		go func() {
			c <- entry.Size()
			close(c)
		}()
	}

	sum := 0
	for _, size := range sizes {
		sum += <-size
	}

	return sum
}

func (d *Dir) mkdir(name string) error {
	if _, exists := d.entries[name]; exists {
		return fmt.Errorf("%s/%s already exists", d.name, name)
	}
	child := newDir(name)
	child.entries[".."] = d
	d.entries[name] = child
	return nil
}

func (d *Dir) touch(name string, size int) error {
	if _, exists := d.entries[name]; exists {
		return fmt.Errorf("%s/%s already exists", d.name, name)
	}
	d.entries[name] = &File{name, size}
	return nil
}

func (d *Dir) cd(name string) (*Dir, error) {
	child, ok := d.entries[name]
	if !ok {
		return nil, fmt.Errorf("%s/%s does not exist", d.name, name)
	}
	if subdir, ok := child.(*Dir); ok {
		return subdir, nil
	}
	return nil, fmt.Errorf("%s/%s is not a directory", d.name, name)
}
