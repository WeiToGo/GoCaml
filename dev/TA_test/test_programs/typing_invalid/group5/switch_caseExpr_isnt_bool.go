package main

func main () {

	var x int = 0
	switch x+=5;{
	
		//error: case expr has non-bool type 
		case x<1,x+2:
			println("x:",x)
		default:
			println("huh")
	}
	
}
