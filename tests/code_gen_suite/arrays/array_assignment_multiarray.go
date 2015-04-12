package main 

func main () {
	var x [3][2]float64
	x[2][1] = 0.5
	println(x[2][1] == float64(0.5))
	println(x[2][0] == float64(0))
}
