package main 

func main() {
	var x int = 100
	// The condition doesn't typecheck since it doesn't have a boolean type
	for x {
		println(x)
		x--
	}
}
