package main

import "sync"

type Map[K comparable, V any] struct {
	m  map[K]V
	mu *sync.Mutex
}

func NewMap[K comparable, V any]() Map[K, V] {
	var mu sync.Mutex
	return Map[K, V]{
		make(map[K]V),
		&mu,
	}
}

func (m Map[K, V]) Set(k K, v V) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.m[k] = v
}

func (m Map[K, V]) Get(k K) (V, bool) {
	m.mu.Lock()
	defer m.mu.Unlock()
	v, ok := m.m[k]
	return v, ok
}
