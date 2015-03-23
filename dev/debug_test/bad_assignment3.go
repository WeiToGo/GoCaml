package main

func main() {	
	var a, b float64 = 1.0, 2.0
	// Invalid since assignment operator accepts integers only
	a &= b
	println(a)
}
