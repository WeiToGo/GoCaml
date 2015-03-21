/* In this file, a type mismatch occurs in the short var decl */
package main

func main () {
	var x bool = true
	if x++; x {
		return
	}
}
