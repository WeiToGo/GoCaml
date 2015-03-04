// "a" is not a lvalue

package main

func main() {
	var x int
	x, "a" = 2, "a"
	if x > 2 {
		print()
	}

}

