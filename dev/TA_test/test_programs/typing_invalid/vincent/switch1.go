package main

func main() {
	type yesno bool
	var yes yesno

	switch {
	case true: println("true")
	case yes: println("yes")
	}
}
