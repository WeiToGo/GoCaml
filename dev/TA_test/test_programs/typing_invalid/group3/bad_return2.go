package main

func mult(a float64, b float64) float64 {
	var result float64 = a * b

	// Invalid since the function returns an int while it return a float64
	return 0
}

func main() {
		// Invalid since the - operator expects a numeric type
    println(mult(5.0,6.5))
}
