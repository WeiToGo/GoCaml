package main 

func main () {
	var x []int
	x = append(x, 3)
	z := x
	z[0] = 9
	println(x[0])
}
