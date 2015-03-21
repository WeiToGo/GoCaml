package main

func main () {

	var x int = 0
	switch x+=5;x{
		case 1,2,3,4:
			println("x:",x)
			
		//invalid case true in switch on x (mismatched types bool and int)
		case true:
			println("it is",5)
		default:
			println("huh")
	}
	
}
