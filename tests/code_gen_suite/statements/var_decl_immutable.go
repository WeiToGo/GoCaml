package main 

func main () {
	var x struct { a int; b int;}
	var y = x
	x.a = 1
	println(y.a)
}
