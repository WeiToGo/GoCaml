package main 

func foo() float64 { 
	return 1.2
}
func main () {
	println("outside the block.")
	var x = 34
	 {
	 	var x = 355
	 	println("inside the block")
	 	println(x)
	 }
	 println(x)
}
