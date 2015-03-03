package main

func main() {
	// The most basic type, with a single condition.
	i := 1
	for i <= 3 {
		println(i)
		i = i + 1
	}

	// A classic initial/condition/after `for` loop.
	for j := 7; j <= 9; j++ {
		println(j)
	}

	// `for` without a condition will loop repeatedly
	// until you `break` out of the loop or `return` from
	// the enclosing function.
	for {
		println("loop")
		break
	}
}
