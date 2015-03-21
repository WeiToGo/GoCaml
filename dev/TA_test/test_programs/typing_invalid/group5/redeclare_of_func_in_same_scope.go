package main

func main(){

	adder(2,3)
}



func adder (a int, b int) {
	var c = a+b
	print(c)
}

//error: adder redeclared in this block, previous declaration at
func adder (a int, b int) {
	var c = a+b+b
	print (c)
}
