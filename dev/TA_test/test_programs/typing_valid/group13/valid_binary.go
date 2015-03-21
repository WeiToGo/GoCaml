package main

/*
 * Tests valid binary operations
 * type of LHS must match that of the RHS
 * as well as operands must be of a certain type or alias
 * depending on the operation
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

	print(i|i)
	print(r|r)
	print(f-f)
	print(i2-i2)
	print(s+s)
	print(b&&b)
	print(s<s)
	print(i2>i2)
	print(f>=f)
	print(r<=r)
	print(arr==arr)
}
