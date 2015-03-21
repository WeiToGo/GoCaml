package main 

func random() int {
	//Suppose this is a random integer
	return 0
}

func main() {
	// The clause doesn't type check because the initial statement doesn't type check
	switch x := 0.0 + random(); x {
		// Doesn't matter
	}
}
