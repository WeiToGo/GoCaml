// Try to append to a slice when:

package main

func main() {
	type slice []int
	type indirect_slice slice
	type elem int

	var ws indirect_slice
	var xs slice
	var ys []int
	var zs []elem

	var x elem
	var y int

	//ws = append(ws, x)
	ws = append(ws, y)

	// xs = append(xs, x)
	xs = append(xs, y)

	// ys = append(ys, x)
	ys = append(ys, y)

	zs = append(zs, x)
	// zs = append(zs, y)
}
