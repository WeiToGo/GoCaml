package main 

func main() {
	var x int
	// The clause doesn't type check because the condition doesn't have a boolean type
	if x:= 0; x {
		println("Bigger than one!")
	} else {
		println("Smaller or equal to one!")
	}
}
