package main 

func main () {
	var x [2]int
	y := x
	x[0] = 2
	println(x[0])
	println(y[0])
}
