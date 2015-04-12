package main 

var s1 struct { x int; y float64; z [2]int; w string; }
var s2 struct { x int; y float64; z [2]int; w string; }
func main () {
	s1.x = 42
	s1.y = 0.3
	s1.z[0] = 34
	s1.z[1] = 35
	s1.w = "kirkkirkkirk"

	s2.x = 42
	s2.y = 0.3
	s2.z[0] = 34
	s2.z[1] = 35
	s2.w = "kirkkirkkirk"
	println (s1 == s2)
	s2.z[0] = 2
	println (s1 == s2)
}
