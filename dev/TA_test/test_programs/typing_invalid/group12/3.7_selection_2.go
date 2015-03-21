// point does not have a field named z
package main

type point struct {
	x, y float64
}

func main() {
	var p point
	p.z = 1.0
	print(p.z)
}
