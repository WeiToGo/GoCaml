package main 

func main () {
	var x [2]int
	var y [2]int
//	x[0] = 7
//	println(x[0])
//	println(x[1])
	println(x == y)
	x = y
	println(x != y)
}
