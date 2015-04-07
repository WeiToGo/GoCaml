package main 

func main () {
// int type
	println(2==3)
	println(2!=3)
	println(2<3)
	println(2<=3)
	println(2>3)
	println(2>=3)
	println(2+3)
	println(2-3)
	println(2*3)
	println(2/3)
	println(20%3)
	println(2|3)
	println(2^3)
	println(2<<3)
	println(2>>3)
	println(2&3)
	// println(2&^3) //to be implemented

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

// bool
	// println(true || false)  // This is still not working
	// Since this is complicated, make a new test case for it. 


}


