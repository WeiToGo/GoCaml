package main

func main() {	
	var a,b int
	var c,d float64
	// Invalid since assignments do not pairwise-typecheck
	a, b, c, d = 0.0, 1, 2.0, 3.0
}
