package main 

func main () {
	var x []float64
	x = append(x, 3.0)
	x = append(x, 5.1)
	println(x[0] == 3.0)
	println(x[1] == 5.1)

	var y []rune
	y = append(y, 'a')
	y = append(y, 'b')
	println(y[0])
	println(y[1])

	var z []bool
	z = append(z, true)
	z = append(z, true)
	println(z[0])
	println(z[1])

	var w []struct { a int; }
	var a struct { a int; }
	a.a = 34
	w = append(w, a)
	w = append(w, a)
	println(w[0].a)
	println(w[1].a)

	var p [][2]int
	var arr [2]int
	p = append(p, arr)
	arr[0] = 4
	arr[1] = 2
	p = append(p, arr)
	println(p[0][0])
	println(p[0][1])
	println(p[1][0])
	println(p[1][1])

	var t [][]int
	var u []int
	u = append(u, 3)
	v := append(u, 5)
	t = append(t, u)
	t = append(t, v)
	println(t[0][0])
	println(t[1][0])
	println(t[1][1])

}
