package main 

type point struct { s int; y int; }

var p point
func main () {
		type silly struct { a string; }
		var s silly

		p.s, p.y, s.a = 123, 8, "boo"
		println(p.s)
		println(p.y)
		println(s.a)
}
