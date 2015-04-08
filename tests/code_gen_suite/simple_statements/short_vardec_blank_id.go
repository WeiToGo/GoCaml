package main 

func foo() int { 
	println("I was evaluated")
	return 0
}
func main () {
	x := 3
	y, _ := 8, foo()

	println(x)
	println(y)
}
