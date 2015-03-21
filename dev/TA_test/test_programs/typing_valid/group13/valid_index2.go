package main

/*
 * Indexing example testing both arrays and slices
 */
func main() {
	var a []int
	var b [10]int

	print(a[1])
	print(b[1])

	a[1] = 2
	b[1] = 2
}
