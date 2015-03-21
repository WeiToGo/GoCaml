// left side of indexing expression must be array/slice

package main

func main() {
	type point struct {
		x, y int
	}
	var p point
	p[0] = 1
	print(p.x)
}
