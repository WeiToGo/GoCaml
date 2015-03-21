package main

func main() {
	type t bool
	var my_bool bool
	var my_t t

	// Bool
	if my_bool {
	}

	if my_bool {
	} else {
	}

	if my_bool {
	} else if my_bool {
	} else {
	}


	// Bool alias
	if my_t {
	}

	if my_t {
	} else {
	}

	if my_t {
	} else if my_t {
	} else {
	}

	// Mix
	if my_t {
	} else if my_bool {
	}

	if x := 1; x == 0 {
	} else if x := 2; x == 1 {
		println(x)
	} else {
		println(x)
	}
}
