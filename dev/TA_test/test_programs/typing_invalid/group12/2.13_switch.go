// type of x does not match type of 'a','b','c'

package main

func main() {
	x := true
	switch x {
	case 'a', 'b', 'c':
		print()
	}
}
