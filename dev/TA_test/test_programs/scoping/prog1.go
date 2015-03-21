package main

type point struct {
	x, y, z float64
}

func main() {
	var p point
	var y float64
	{
		type p int
		var x p = p(3)
		var y p = x + x
		println(y)
	}
	p.y = y
}
