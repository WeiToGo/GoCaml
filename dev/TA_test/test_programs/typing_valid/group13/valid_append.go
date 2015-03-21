package main

/*
 * Tests append type checking
 * first argument is of type Slice<T> or Array<T> (or alias) and second
 * argument is of type T
 */
func main() {
	var a []int
	append(a, 1)
}
