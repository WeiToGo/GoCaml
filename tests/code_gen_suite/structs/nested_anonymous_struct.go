package main 

type length int
type point struct { x int; y int; }
type ray struct { p1 struct { x int; y int; }; l length ;} 

func main () {
		var l ray
		l.p1.y = 5 

		println(l.p1.x)
		println(l.p1.y)
		println(l.l)
}
