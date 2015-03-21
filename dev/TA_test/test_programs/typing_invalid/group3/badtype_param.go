package main

func factorial(x int) int {
	var total int = 1;
	var iter int;
	for iter = 2; iter <= x; iter++ {
		total *= iter;
	} 
	return total;
}

func main() {
	// Call the factorial function with a bad parameter type
	var fac int = factorial(10.23)
	println(fac)
	return;
}
