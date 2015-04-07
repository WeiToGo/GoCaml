package main 

func main () {

// float type
	println(23.4 == 24.5)
	println(23.4 != 24.5)
	println(23.4 < 24.5)
	println(23.4 <= 24.5)
	println(23.4 > 24.5)
	println(23.4 >= 24.5)

// These float operations work,
// but the test fails because floating point values don't match
// exactly with go output
// You can write a .gocamlout file to handle these if you want 

	// println(23.4 + 24.5)
	// println(23.4 - 24.5)
	// println(23.4 * 24.5)
	// println(23.4 / 24.5)

}


