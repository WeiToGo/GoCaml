// cannot assign string to int

package main

func main() {
	var x, y int
	x, y, z := 2, "2", 2
	print(x, y, z)
}
