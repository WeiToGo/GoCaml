package main 

func main () {
	var x []int
	x = append(x, 3)
	y := append(x, 5)
	y[0] = 7
	println("The following should raise an java out of bounds exception.")
	println(x[1])
}
