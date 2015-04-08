package main 


func foo() int { 
	return 1
}

func bar() { 
	println("I got printed");
	return
	println("I should not be printed");
}

func float_return() float64 {
	return 4.5
}

func string_return() string{
	return "a string"
}

// func rune_return() rune { 
// 	return 's'
// }

func main () {
	x := foo()
	bar()
	println(4.5 == float_return())
	println(x)
	println(string_return())
	// println('s' == rune_return())

}
