package main

func main() {

	//array
	var a [5]int
	a[4] = 100
	
	//slice
	var b [] int
	var c int
	
	//indexing ok
	b[1] = 100
	
	//error, c is not array nor slice
	c[1] = 100 

}
