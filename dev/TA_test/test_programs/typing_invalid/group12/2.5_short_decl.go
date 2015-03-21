// no new variables on left side of :=

package main

func main() {
	var x, y int
	x, _ := 2, 2
	print(x, y)
}

