// cannot convert string to type rune

package main

func main() {
	s := "s"
	var x rune = rune(s)
	print(x)
}
