package main 

func main () {
	var x, y int
	var array [2][2]int

	x = 3
	x, y = 8, x+1

	array[0][0] = 4*x+3
	array[1][1] = y
	println(x)
	println(y)
	println(array[0][0])
	println(array[1][1])
}
