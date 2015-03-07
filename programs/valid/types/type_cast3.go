package main 

func f() {
	var a struct {
		x, y float64
	}
	print(int(a.x))
}
