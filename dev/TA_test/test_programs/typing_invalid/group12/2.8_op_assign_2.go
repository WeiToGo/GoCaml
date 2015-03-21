// mismatched types float64 and int

package main

func main() {
	x, y := 1.1, 1
	x -= y
	print(x)
}
