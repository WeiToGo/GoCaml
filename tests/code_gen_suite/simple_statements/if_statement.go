package main 

 var x int
func main () {
	if true {
		println("if")
	}

	// if/then with empty init statement
	if ; true {
		println("if")
		println("if again")
	}

	// if/then with init statement
	if x := 0; x == 0 {
		println("if")
		println("if again")
	}

	// if/then/else
	if false {
		println("if false stmt")
	} else {
		println("else true stmt")
	}

	// // if/then/else with empty init statement
	if ; true {
		println("if false stmt")
	} else {
		println("if false stmt")
	}

	// // if/then/else with init statement
	if x=4; true {
		println(x)
	} else {
		println("if false stmt")
		println("if false 2 stmt")
	}

	// if/else-if/else
	if false {
		println("if false  stmt")
	} else if false {
		println("if false 2 stmt")
	} else {
		println("if true stmt")
	}

	// // if/else-if/else with init statement
	if false {
		println("if false  stmt")
		println("if false 2 stmt")
	} else if x; false {
		println("if false 2 stmt")
	} else {
		if false {
			println("if false  stmt")
		} else if false {
			println("if false 2 stmt")
		} else {
			println("if true stmt")
		}	
	}
}
