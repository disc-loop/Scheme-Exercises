package main

import (
	"assert"
	"testing"
)

func Testf(t *testing.T) {
	assert.Equal(t, f(0), 0)
	assert.Equal(t, f(1), 1)
	assert.Equal(t, f(2), 2)
	assert.Equal(t, f(3), 4)
	assert.Equal(t, f(4), 11)
	assert.Equal(t, f(5), 25)
}
