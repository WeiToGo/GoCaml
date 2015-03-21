package main

type v int
var yu v = v(0)

func f(x,y int) [3]int{
	var a [3]int
	a[0] = 0
	a[1] = 1
	a[2] = 2
	return a
}


func main() {
var v float64
	{
		var x float64 = float64(yu)+v
		var v int
		println(x,v)
	}


	type a int
	type c a
	type b int
	var x c
	var y int

	x = c(a(5))
	x = c(5)
	
	y = int(a(x))
	y = int(x)

	x = c(5)

	type d [3]a
	type ddd [3]d
	var x1,y1,z1 []ddd
	type e d

	var ar e
	ar[0] = a(3);
	ar[0+2] = a(38)
	//ar[1] = f(0,0)[1]

	type dd struct{
		x,y int
		z ddd
	}

	type ee dd
	var s dd
	s.x = 3
	s.z[2][2] = a(3)
}


