package main

func main(){

	adder(2,3)
}

//error:missing return at end of function
func adder (a int, b int) int {
	var c = a+b
	print(c)
}
