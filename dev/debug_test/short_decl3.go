package main

func main() {
	a, _ := 0, 1
	// No new variable on lhs
	a, _ := 2, 3
	println(a)
}
