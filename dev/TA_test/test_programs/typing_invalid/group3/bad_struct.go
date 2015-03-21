package main

struct person {
	name string
	age int
}

func main() {
	// This doesn't type check because the struct person doesn't have a lastname field
	bob := person{"Bob", 36}
	println(bob.lastname)
}
