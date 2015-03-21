// double already declared in current scope

package main

var double bool = false
type double int

func main() {
	var x double = 2
	print(x)
}
