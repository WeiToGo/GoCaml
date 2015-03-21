package main 

func main() {
	// The clause doesn't type check because the init expression doesn't type check
	if x:= 0.0; x > 1 {
		println("Bigger than one!")
	} else {
		println("Smaller or equal to one!")
	}
}
