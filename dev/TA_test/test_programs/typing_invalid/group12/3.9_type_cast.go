// cannot type cast something to alias of string
// note: this typechecks in Go

package main

type str string

func main() {
	var s str = str('a')
	print(s)
}
