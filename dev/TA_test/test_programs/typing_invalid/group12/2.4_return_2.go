// type of "str" doesn't match return type of f

package main

func f() rune {
	return "str"
}

func main() {
	print(f())
}
