package main

import "fmt"

func main() {
	f(3)
}

func f(n int) int {
	a, b, c := 2, 1, 0
	for i := n; i >= 2; i-- {
		c = b
		b = a
		a = a + (2 * b) + (3 * c)
		fmt.Println(a)
	}
	return a
}
