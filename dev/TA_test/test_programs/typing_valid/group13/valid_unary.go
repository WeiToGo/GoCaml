package main

/*
 * Valid example of unary operations on a variety of types
 * similar to binary operations, each operation is valid only on
 * certain types (or their aliases)
 */
func main() {
	type int2 int;

	var b bool
	var i int
	var i2 int2
	var f float64
	var r rune
	var s string
	var arr [5]int
	var slice []int

	print(+i, +i2, +f, +r, +arr[0])
	print(-i, -i2, -f, -r, -arr[0])
	print(!b)
	print(^r, ^i2, ^i)
}
