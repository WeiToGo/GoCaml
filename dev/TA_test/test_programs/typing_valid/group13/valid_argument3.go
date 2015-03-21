package main

/*
 * Simple example of a function with void parameters, but a return type
 * Note that print is a polymorphic fucntion and can except a variety of types
 */
func test() float64 {
	return 5.5
}

func main() {
	print(test())
}

