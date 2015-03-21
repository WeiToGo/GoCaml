package main 

func main() {
	var x int = 100
	// The condition doesn't typecheck since it compares an int and a bool
	for x > 0.0 {
		println(x)
		x--
	}
}
