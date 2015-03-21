/* In this file, we attempt to append a bool to an int slice */
package main

func main () {
	var s []int
	var x bool = true
	append(s, x)
}
