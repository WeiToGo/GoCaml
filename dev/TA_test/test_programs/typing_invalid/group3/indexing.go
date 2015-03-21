package main

func main() {
	a := [5]int{0,1,2,3,4}
	// This does not type check, the array index must be an int or a slice
	println(a['0'])
}
