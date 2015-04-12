package main 

var s1 struct { x int; y int;}
var s2 struct { x int; y int;}
func main () {
	println (s1 == s2)
	s1.x = 5
	println (s1 == s2)
	s2.x = 5
	println (s1 == s2)
}
