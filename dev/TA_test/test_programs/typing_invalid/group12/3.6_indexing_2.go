// index must have type int

package main

func main() {
	var a [3]int
	x := 2.5
	a[x] = 0
	print(a[0], x)
}
