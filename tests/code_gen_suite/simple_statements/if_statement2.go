package main 

var x int
func main () {

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
	} else if false {
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
