package main

func add(a int, b int) float64 {
	return (float64)(a + b);
}

func main() {	
	var a,b int
	// Invalid since one of the right-hand expressions does not typecheck
	a, b = add(1,2), 3 
}
