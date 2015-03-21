package main

func qwerty() bool {
	type my_bool bool
	var p my_bool = my_bool(false)
	return p
}

func main() {
	qwerty()
}
