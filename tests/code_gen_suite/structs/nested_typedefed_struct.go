package main 


type point struct { x int; y int; }
type line struct { p1 point; p2 point; } 

func main () {
		var l line
		l.p1.y = 5 

		println(l.p1.x)
		println(l.p1.y)
		println(l.p2.x)
		println(l.p2.y)
}
