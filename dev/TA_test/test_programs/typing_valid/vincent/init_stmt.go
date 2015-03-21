package main

func main() {
	if x := 0; x == 0 {
		x++
	}

	if x := 0; x == 0 {
		x++
	} else if y := 1; y < 0 {
		y++
	}

	switch x := 0; x {
	case 0: x++
	default: x = 0
	}

	for x := 0; x < 10; x++ {
		x++
	}
}
