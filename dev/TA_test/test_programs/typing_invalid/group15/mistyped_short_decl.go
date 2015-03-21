/* In this file, a type mismatch occurs in the short var decl */
package main

func main () {
	var v1 int
	v1, v2, v3 := true, 2, 3
	return
}
