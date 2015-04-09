package main 

var s1 struct { x int; y int;}
var s3 struct { x int; y int;}
func main () {
	var s2 struct { y int; x int;} 
	var s1 struct { y int; w struct { x int; y int; };} 

}
