package main 

type length int
type point struct { s length; y int; }

var p point
func main () {
		type silly struct { a string; }
		var s silly

		p.y, s.a = 8, "boo"
		println(p.s)
		println(p.y)
		println(s.a)
}
