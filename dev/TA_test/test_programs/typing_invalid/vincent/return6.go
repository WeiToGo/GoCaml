package main

func f() int {
	if true {
		return 3
	} else {
		// Return expression is not an int
		return "string"
	}
}
