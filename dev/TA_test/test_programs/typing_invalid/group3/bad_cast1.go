package main

struct person {
	name string
	age int
}

func main() {
	// This doesn't type check because a struct cannot be typecast
	bob := person{"Bob", 36}
	println((int)bob)
}
