package main

func average(x []float64) float64 {
	// Uncommented this to cause an undeclared variable error
	// var value float64

	total := 0.0
	for i := 0; i < len(x); i++ {
		value = x[i]
		total += value
	}

	return total / float64(len(x))
}

func main() {
	var x []float64
	println(average(x))
}
