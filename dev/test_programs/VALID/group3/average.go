package main

func main() {
	var x []float64
	x = append(x, 1)
	x = append(x, 2)
	x = append(x, 3)
	println(average(x))
}

func average(x []float64) float64 {
	var value float64

	total := 0.0
	for i := 0; i < len(x); i++ {
		value = x[i]
		total += value
	}

	return total / float64(len(x))
}
