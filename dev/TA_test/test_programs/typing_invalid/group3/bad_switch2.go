package main 

func random() int {
	//Suppose this is a random integer
	return 0
}

func main() {
	var x int
	// The switch block doesn't type check because the first case doesn't type check
	switch x = random(); x {
		case 0, "0", 0.0, false:
			println("It's a ZERO!")
		default:
			println("It's not a zero... What is it?")
	}
}
