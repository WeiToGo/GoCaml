package main


var x int = 6
var c int

func main () {


	switch c=5; {
	case x < 0: println("negative")
	case x > 0: println("positive")
	default: println("zero")
	}

	switch {
	case x < 0: println("negative")
	case x > 0: println("positive")
	default: println("zero")
	}

	// switch, default in the middle-- works
	switch c=3; x {
	case 0: println("zero")
	default: println("something else")
	case 1, 3, 5, 7, 9: println("odd")
	}
}
