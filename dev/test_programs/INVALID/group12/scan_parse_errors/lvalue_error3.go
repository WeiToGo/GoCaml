// x*y is not a lvalue

package main;

func main() {
	var x,y int
	x*y = 5
	if x>2 {
		print()
	}

}