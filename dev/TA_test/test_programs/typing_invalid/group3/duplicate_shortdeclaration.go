package main

func main() {	
	var a,b int
	var c,d float64
	// Invalid since all left-hand variables are declared in current scope
	a, b, c, d := 0, 1, 2.0, 3.0
}
