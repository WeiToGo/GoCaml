package main 

func main () {
	var x [3]struct {a int; b int;}
	var y struct { a int; b int;}
	y.b = 5
	x[1] = y
	y.b = 6
	println(y.b)
	println(x[1].b)
	println(x[0].b)
	println(x[0].a)
	println(x[1].a)
}
