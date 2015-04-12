package main 

func foo(a [2]int) { 
	a[0] = 3
}
func main () {
	var x [2]int;
	foo(x);
	println(x[0])
}
