package main 

var a1 [2] int;
var a2 [2] int;

var b1 [2] float64
var b2 [2] float64

var c1 [2] rune
var c2 [2] rune

var d1 [2] bool
var d2 [2] bool

var s1 [2]struct { a [2]int; }
var s2 [2]struct { a [2]int; }

func main () {
	println(a1 == a2)
	println(b1 == b2)
	println(c1 == c2)
	println(d1 == d2)
	println(s1 == s2)
	
	println(a1 != a2)
	println(b1 != b2)
	println(c1 != c2)
	println(d1 != d2)
	println(s1 != s2)

	s1[0].a[1] = 5
	println(s1 == s2)
	println(s1 != s2)
}
