package main 

func main() {
	var x int
	// The clause doesn't type check because the initializing statement doesn't type check
	for x = 0.0; x < 100; x++ {
		println(x)
	}
}
