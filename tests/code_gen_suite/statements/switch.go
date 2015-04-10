package main


var x int = 6
var c int

func main () {
	switch 10 {
	case 0, 1 : println("one")
	case 4: println("four")
	default: println ("zero")
	}

	switch 5 {
	case 0, 1 : println("one")
	case 4, 5, 6: println("four")
	default: println ("zero")
	}

	switch 24 {
	case 0, 1 : println("one")
	default: println ("zero")
	case 4, 5, 6: println("four")
	}

}
