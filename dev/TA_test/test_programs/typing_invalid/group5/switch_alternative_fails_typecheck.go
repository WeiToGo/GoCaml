package main

func main () {

	var x int = 0
	switch x+=5;x{
		case 1,2:
			println("x:",x)
			
		//statements under alternative doesn't type check
		case 5:
			println("it is"+5)
		default:
			println("huh")
	}
	
}
