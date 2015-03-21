package main

func id(x int) int {
	return x
}

func main() {
	var p struct {
		x, y int
	}

	p.x = id
}
