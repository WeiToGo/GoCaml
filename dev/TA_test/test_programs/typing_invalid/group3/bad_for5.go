package main 

func main() {
	var x int
	// The clause doesn't type check because the post expression doesn't type check
	for x = 10; x > 0; x -= 1.0 {
		println(x)
	}
}
