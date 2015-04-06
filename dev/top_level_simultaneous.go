package main

func foo() int { 
	return 1
	}

var x, y, z = 2, w, foo()

func main() {
	println(x, y, z)
}
