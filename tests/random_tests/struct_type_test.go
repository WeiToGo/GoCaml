package main 

var s1 struct { x int; y int;}
var s2 struct { y int; x int;} 
func main () {
	s1 = s2
}
