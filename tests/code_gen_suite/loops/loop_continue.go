package main 

func main () {
	for x:= 0; x < 10; x = x + 1 { 
		println(x)
		continue
		println("I should never be printed")
	}
}
