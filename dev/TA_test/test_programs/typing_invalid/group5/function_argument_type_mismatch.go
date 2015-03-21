package main

func adder (a int, b float64) int{
	var c = a+int(b)
	print(c)
	return c
}

func main(){

	//cannot use "a" (type string) as type float64 in argument to adder
	adder(2,"a")
}
