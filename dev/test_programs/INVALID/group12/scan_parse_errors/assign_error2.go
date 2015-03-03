// For op-assign statements, only a single id and single expression can be on either side of the assignment operator.

package main

func main() {
	var x, y int
	x, y = 10, 10
	if x>2 && y>2 {
		x, y -= 5
	}

}