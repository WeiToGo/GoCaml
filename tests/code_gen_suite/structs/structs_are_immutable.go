package main 

type point struct { x int; y int; }

var p point
func main () {
		var p point; 
		var q point = p;
		p.x = 1
		q.x = 2
		println(p.x)
		println(q.x)
}
