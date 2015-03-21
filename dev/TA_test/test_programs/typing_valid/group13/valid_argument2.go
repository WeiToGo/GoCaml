package main

/*
 * Valid example of a function with a return type, where the return
 * type is then used in an assignment statement. The return type and the
 * variable's type must be the same
 */
func test(a, b int) float64 {
	return 5.5
}

func main() {
	var a float64
	a = test(1, 2)
	print(a)
}

