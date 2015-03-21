package main

type point struct {
	x, y int
}

func three_four() point {
	var p point
	p.x = 3
	p.y = 4
	return p
}

func main() {
	println(three_four().x)
}
