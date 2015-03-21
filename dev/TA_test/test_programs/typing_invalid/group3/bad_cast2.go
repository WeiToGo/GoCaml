package main

struct person {
	name string
	age int
}

func main() {
	// This doesn't type check because you cannot typecast to a struct
	bob := "Bob is 36 years old"
	println((person)bob)
}
