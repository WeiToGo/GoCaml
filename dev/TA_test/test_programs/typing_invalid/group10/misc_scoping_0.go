package main

type a int
type b a
type c b

type str struct {
	x, y int
}



func main() {

	var x int
	var v b = 3
	x = int(v)
	v = b(x) + 1
	println(x,v)

	var a int 
	b,a := 1,2
	println(a,b)

	v = b(x) + 1

}
