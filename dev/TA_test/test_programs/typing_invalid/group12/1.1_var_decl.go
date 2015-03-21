// x already declared in current scope

package main

func f(x int) {
	var x bool = true
	print(x)
}

func main() {
	f(1)
}
