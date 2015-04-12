package main 

func main () {
	var x [3][2]struct { a float64; b int; c string; d rune; }
	println(x[0][0].a == float64(0))
	println(x[0][1].b)
	println(x[1][1].c)
	println(x[2][0].d)
}
