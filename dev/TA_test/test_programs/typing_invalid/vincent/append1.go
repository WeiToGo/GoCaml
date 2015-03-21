package main

func main() {
	type slice []int
	type elem int

	var xs slice
	var ys []int
	var zs []elem

	var x elem
	var y int

	// Cannot append an `elem` to a `slice` ([]int)
	xs = append(xs, x)
	xs = append(xs, y)

	// ys = append(ys, x)
	ys = append(ys, y)

	zs = append(zs, x)
	// zs = append(zs, y)
}
