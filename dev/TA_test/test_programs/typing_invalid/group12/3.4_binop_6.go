// mismatched types double and int

package main

type double float64

func main() {
	var x double = 2.2
	y := 1
	print(x^y)
}

