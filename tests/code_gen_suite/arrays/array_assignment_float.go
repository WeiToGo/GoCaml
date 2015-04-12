package main 

func main () {
	var x [3]float64
	x[2] = 0.2
	println(x[1] == float64(0))
	println(x[2] == 0.2)
}
