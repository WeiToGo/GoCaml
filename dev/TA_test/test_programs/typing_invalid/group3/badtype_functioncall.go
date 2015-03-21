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
	// Assign the result of the function call to a variable of a different type
	var fac string
	fac = factorial(10)
	println(fac)
	return;
}
