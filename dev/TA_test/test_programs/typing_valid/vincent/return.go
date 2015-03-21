package main

func incr(x float64) float64 {
	return x + 1.0
}


type point struct {
	x, y, z float64
}

func new_point() point {
	var p point
	p.x = incr(-1.0)
	p.y = 0.0
	p.z = 0.0
	return p
}
