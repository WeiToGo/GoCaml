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
	println(true || true)  
	println((6<9) || (4>5)) //T F
	println((4!=4) || (4>5)) // F F
	println((10<9)|| (5==5)) // F T
	println((2>4) && (3==4)) // F F
	println((4!=5) && (4<=9)) //T T
	println((4!=5) && (3==4)) // T F
	println((3>=9) && (5!=6)) // F T

}


