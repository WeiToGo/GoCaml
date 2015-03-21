package main

func average(data []float64, length int) float64 {
	sum := 0.0
	for i := 0; i < length; i++ {
		sum += data[i]
	}
	return (sum / float64(length))
}

func variance(data []float64, length int) float64 {
	sum := 0.0
	avg := average(data, length)
	for i := 0; i < length; i++ {
		sum += (data[i] - avg) * (data[i] - avg)
	}
	return (sum / float64(length))
}

func main() {
	var data []float64
	length := 8
	data = append(data, 2.5)
	data = append(data, 3.)
	data = append(data, 5.)
	data = append(data, 2.1)
	data = append(data, 1.)
	data = append(data, 0.5)
	data = append(data, 3.)
	data = append(data, .7)
	println("data:")
	for i := 0; i < length; i++ {
		println(data[i])
	}
	println("average:")
	println(average(data, length))
	println("variance:")
	println(variance(data, length))

}
