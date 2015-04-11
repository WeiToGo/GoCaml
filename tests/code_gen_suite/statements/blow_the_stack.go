package main 

func foo() float64 { 
	return 1.2
}
func main () {
	for x:= 0; x < 1000; x = x + 1 { 
		foo()
	}
	println("Loop finished!")
}
