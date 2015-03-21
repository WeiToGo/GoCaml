package main 

func main() {
	var x int
	// The clause doesn't type check because the terminating condition doesn't have type bool
	for x = 10; x; x-- {
		println(x)
	}
}
