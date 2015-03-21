package main

/*2.7 Assignment*/

func main(){
	
	var x int = 0
	var y float64= 1.0
	var a,b int
	
	//error: not the same corresding type on both sides
	//cannot use y * y (type float64) as type int in assignment
	
	a,b = x*x , y*y
	println(a,b)
}
