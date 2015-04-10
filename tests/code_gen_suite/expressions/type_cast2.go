package main

type length rune
type er int

var x length = length('5')
var y length = length('3') 

var z er = er(5)
var q er = er(6)

func main() {
	println(x+ y)
}
