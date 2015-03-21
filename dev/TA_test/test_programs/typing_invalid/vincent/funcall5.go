package main

func f() int {
	return 0
}

func main() {
	// f() is an int, not a float64
	var x float64 = f()
}
