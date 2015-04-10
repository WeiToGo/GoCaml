package main


var x int
var c int

func main () {
	
	switch "stre" {
	case "awe", "se" : println("one")
	default: println ("zero")
	case "pa", "qw", "ad": println("four")
	}

	switch x = 4 ; 'e' {
	case 'w' : println("negative")
	case 'e': println("positive")
	default: println("zero")
	}
	
	switch 4.5 {
	case 2.76 : println("negative")
	case 5.44 : println("positive")
	default: println("zero")
	}
	
}
